#!/usr/bin/env python3
"""참여자가 렌더한 HTML을 모아 대시보드 사이트를 만든다.

하는 일은 셋뿐이다.
  1. projects/*/_master.html 을 훑어 현황표(dashboard/_status.md)를 쓴다
  2. dashboard/index.qmd 를 렌더한다  → 대시보드 첫 페이지
  3. 결과와 참여자 HTML을 docs/ 로 모은다

대시보드 내용을 바꾸고 싶으면 이 파일이 아니라 dashboard/index.qmd 와
dashboard/dashboard.css 를 고치면 된다.

사용법:
    python3 scripts/build_site.py
"""
from __future__ import annotations

import html
import re
import shutil
import subprocess
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

# ── 설정 ────────────────────────────────────────────────────────
# 폴더 이름을 바꿨다면 여기만 고치면 된다.

PAPERS_DIR = "projects"            # 참여자 폴더들이 있는 곳
OUTPUT_DIR = "docs"                # 완성된 사이트가 놓일 곳
DASHBOARD  = "dashboard/index.qmd" # 대시보드 원본

SOURCE   = "_master.qmd"
RENDERED = "_master.html"
HYPOTHESIS = '<script src="https://hypothes.is/embed.js" async></script>'

ROOT = Path(__file__).resolve().parents[1]
PAPERS = ROOT / PAPERS_DIR
SITE = ROOT / OUTPUT_DIR
DASH_QMD = ROOT / DASHBOARD
KST = timezone(timedelta(hours=9))


# ── 참여자 폴더 훑기 ────────────────────────────────────────────

def read_head(path: Path, limit: int = 60000) -> str:
    return path.read_text(encoding="utf-8", errors="replace")[:limit]


def meta_from_html(path: Path) -> dict:
    head = read_head(path)
    out = {}
    m = re.search(r"<title>(.*?)</title>", head, re.S | re.I)
    if m:
        out["title"] = html.unescape(re.sub(r"\s+", " ", m.group(1))).strip()
    m = re.search(r'<meta\s+name="author"\s+content="(.*?)"', head, re.I)
    if m:
        out["author"] = html.unescape(m.group(1)).strip()
    return out


def meta_from_qmd(path: Path) -> dict:
    text = read_head(path, 4000)
    m = re.match(r"^---\s*\n(.*?)\n---\s*\n", text, re.S)
    out = {}
    if m:
        for line in m.group(1).splitlines():
            kv = re.match(r'^(title|author):\s*"?(.*?)"?\s*$', line)
            if kv and kv.group(2):
                out[kv.group(1)] = kv.group(2)
    return out


def git_time(path: Path) -> str:
    res = subprocess.run(
        ["git", "log", "-1", "--pretty=format:%cI", "--", str(path)],
        cwd=ROOT, capture_output=True, text=True,
    )
    return res.stdout.strip() if res.returncode == 0 else ""


def scan() -> list[dict]:
    rows = []
    if not PAPERS.is_dir():
        print(f"경고: '{PAPERS_DIR}' 폴더가 없습니다. 스크립트 상단 PAPERS_DIR 을 확인하세요.")
        return rows
    for folder in sorted(p for p in PAPERS.iterdir()
                         if p.is_dir() and not p.name.startswith(".")):
        src, out = folder / SOURCE, folder / RENDERED
        if not src.exists() and not out.exists():
            continue
        meta = meta_from_html(out) if out.exists() else meta_from_qmd(src)
        iso = git_time(out if out.exists() else src)
        when, days = "—", 9999
        if iso:
            dt = datetime.fromisoformat(iso).astimezone(KST)
            when = dt.strftime("%m/%d %H:%M")
            days = (datetime.now(KST) - dt).days
        rows.append({
            "slug": folder.name,
            "ready": out.exists(),
            "title": meta.get("title") or folder.name,
            "author": (meta.get("author") or folder.name).split("(")[0].strip(),
            "when": when,
            "days": days,
        })
    return rows


# ── 1. 현황표 쓰기 ──────────────────────────────────────────────

def write_status(rows: list[dict]) -> None:
    lines = ["| 참여자 | 원고 | 마지막 갱신 | 상태 |", "|:--|:--|:--|:--|"]
    for r in rows:
        title = r["title"]
        if r["ready"]:
            cell = f'[{title}](papers/{r["slug"]}/index.html)'
            if r["days"] <= 3:
                pill = '[새 버전]{.pill .new}'
            elif r["days"] <= 14:
                pill = f'[{r["days"]}일 전]{{.pill .old}}'
            else:
                pill = f'[{r["days"]}일 경과]{{.pill .old}}'
        else:
            cell = f'[{title}]{{.dim}}'
            pill = '[준비 중]{.pill .none}'
        lines.append(f'| {r["author"]} | {cell} | [{r["when"]}]{{.dim}} | {pill} |')

    if not rows:
        lines.append("| — | 아직 등록된 원고가 없습니다 | — | — |")

    ready = sum(1 for r in rows if r["ready"])
    now = datetime.now(KST)
    lines += ["", f'[원고 {ready}/{len(rows)}편 게시됨 · 자동 갱신 '
                  f'{now:%Y년 %m월 %d일 %H:%M} KST]{{.dim}}']
    (DASH_QMD.parent / "_status.md").write_text("\n".join(lines) + "\n", encoding="utf-8")


# ── 2. 대시보드 렌더 ────────────────────────────────────────────

def render_dashboard() -> Path | None:
    if not DASH_QMD.exists():
        print(f"경고: {DASHBOARD} 가 없습니다.")
        return None
    res = subprocess.run(
        ["quarto", "render", DASH_QMD.name],
        cwd=DASH_QMD.parent, capture_output=True, text=True,
    )
    out = DASH_QMD.with_suffix(".html")
    if res.returncode != 0 or not out.exists():
        print("대시보드 렌더 실패:")
        print((res.stderr or res.stdout).strip()[-1200:])
        return None
    return out


def fallback_index(rows: list[dict]) -> str:
    """Quarto가 없거나 렌더가 실패해도 사이트는 나오도록 하는 최소 페이지."""
    items = "\n".join(
        f'<li><a href="papers/{r["slug"]}/index.html">{html.escape(r["title"])}</a>'
        f' — {html.escape(r["author"])} ({r["when"]})</li>' if r["ready"]
        else f'<li>{html.escape(r["title"])} — {html.escape(r["author"])} (준비 중)</li>'
        for r in rows
    )
    return (
        '<!DOCTYPE html><html lang="ko"><head><meta charset="utf-8">'
        "<title>머신러닝 논문 연구모임 2026</title></head><body>"
        "<h1>머신러닝 논문 연구모임 2026</h1>"
        f"<ul>{items}</ul>{HYPOTHESIS}</body></html>"
    )


# ── 3. docs/ 조립 ───────────────────────────────────────────────

def inject_hypothesis(path: Path) -> None:
    """참여자 HTML에 코멘트 기능을 붙인다. 참여자 qmd는 건드리지 않는다."""
    text = path.read_text(encoding="utf-8", errors="replace")
    if "hypothes.is/embed.js" in text:
        return
    text = text.replace("</body>", HYPOTHESIS + "\n</body>", 1) if "</body>" in text \
        else text + HYPOTHESIS
    path.write_text(text, encoding="utf-8")


def main() -> int:
    rows = scan()
    write_status(rows)

    dash_html = render_dashboard()

    shutil.rmtree(SITE, ignore_errors=True)
    SITE.mkdir(parents=True, exist_ok=True)

    if dash_html:
        shutil.copy2(dash_html, SITE / "index.html")
        inject_hypothesis(SITE / "index.html")
        side = dash_html.with_name(dash_html.stem + "_files")   # embed-resources를 끈 경우
        if side.is_dir():
            shutil.copytree(side, SITE / side.name, dirs_exist_ok=True)
    else:
        (SITE / "index.html").write_text(fallback_index(rows), encoding="utf-8")
        print("→ 최소 페이지로 대체했습니다.")

    placed, skipped = 0, []
    for r in rows:
        src = PAPERS / r["slug"] / RENDERED
        if not src.exists():
            skipped.append(r["slug"])
            continue
        dest = SITE / "papers" / r["slug"]
        dest.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src, dest / "index.html")
        inject_hypothesis(dest / "index.html")

        side = PAPERS / r["slug"] / "_master_files"
        if side.is_dir():
            shutil.copytree(side, dest / "_master_files", dirs_exist_ok=True)
        out_dir = PAPERS / r["slug"] / "outputs"
        if out_dir.is_dir() and any(p.name != ".gitkeep" for p in out_dir.iterdir()):
            shutil.copytree(out_dir, dest / "outputs", dirs_exist_ok=True)
        placed += 1

    print(f"{OUTPUT_DIR}/ 생성 완료: 원고 {placed}편 게시 / 참여자 {len(rows)}명")
    if skipped:
        print("건너뜀(렌더된 HTML 없음): " + ", ".join(skipped))
    return 0


if __name__ == "__main__":
    sys.exit(main())
