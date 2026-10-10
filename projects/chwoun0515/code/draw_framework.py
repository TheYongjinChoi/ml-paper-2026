"""Render the agreed research workflow as reusable PNG and SVG figures."""
from pathlib import Path
from html import escape
from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'figures'
OUT.mkdir(exist_ok=True)
W, H, SCALE = 1400, 1940, 2
im = Image.new('RGB', (W*SCALE, H*SCALE), 'white')
d = ImageDraw.Draw(im)
svg = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">',
       '<title>상권 내부 특성과 외부 연대성의 연구 프레임워크</title>',
       '<desc>2021~2025년 개별 상권의 자료 구성, 관계 분석과 미래 예측, 양적·질적 결과 종합의 세 층 연구 수행 순서.</desc>',
       f'<rect width="{W}" height="{H}" fill="white"/>']
INK, MUTED, LINE = '#172C40', '#4E6172', '#8193A2'
BLUE, TEAL = '#EDF3F9', '#EDF6F3'

def font(size, bold=False):
    return ImageFont.truetype('C:/Windows/Fonts/malgunbd.ttf' if bold else 'C:/Windows/Fonts/malgun.ttf', size*SCALE)

def text(x, y, s, size=24, bold=False, fill=INK, center=False):
    f = font(size, bold)
    length = d.textlength(s, font=f)/SCALE
    assert length <= W-100, (s, length)
    px = x-length/2 if center else x
    d.text((px*SCALE,y*SCALE), s, font=f, fill=fill, anchor='lt')
    anchor = 'middle' if center else 'start'
    svg.append(f'<text x="{x}" y="{y}" dominant-baseline="text-before-edge" text-anchor="{anchor}" font-family="Malgun Gothic, 맑은 고딕, sans-serif" font-size="{size}" font-weight="{700 if bold else 400}" fill="{fill}">{escape(s)}</text>')

def rect(x,y,w,h,fill='white',stroke='#CCD6DE',radius=12):
    d.rounded_rectangle((x*SCALE,y*SCALE,(x+w)*SCALE,(y+h)*SCALE),radius*SCALE,fill=fill,outline=stroke,width=2)
    svg.append(f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="{radius}" fill="{fill}" stroke="{stroke}"/>')

def line(points, arrow=False, color=LINE):
    d.line([(x*SCALE,y*SCALE) for x,y in points],fill=color,width=3*SCALE)
    svg.append(f'<polyline points="{" ".join(f"{x},{y}" for x,y in points)}" fill="none" stroke="{color}" stroke-width="3"/>')
    if arrow:
        x,y=points[-1]
        pts=[(x-7,y-12),(x+7,y-12),(x,y)]
        d.polygon([(a*SCALE,b*SCALE) for a,b in pts],fill=color)
        svg.append(f'<polygon points="{" ".join(f"{a},{b}" for a,b in pts)}" fill="{color}"/>')

def header(y,n,title):
    rect(55,y,1290,54,INK,INK,7)
    text(78,y+11,f'LAYER {n}   {title}',27,True,'white')

def merge(yfrom,yto):
    line([(370,yfrom),(370,yfrom+18),(1030,yfrom+18),(1030,yfrom)])
    line([(700,yfrom+18),(700,yto)],True)

text(700,30,'상권 내부 특성과 외부 연대성의 연구 프레임워크',39,True,center=True)
text(700,89,'개별 상권 · 2021~2025년  |  민감성과 대응력을 구분하여 분석',24,fill=MUTED,center=True)

header(145,1,'상권 내부 특성과 외부 연대성의 자료 구성')
rect(55,218,630,310,BLUE)
rect(715,218,630,310,TEAL)
text(82,239,'상권 내부 특성',29,True)
text(82,288,'민감성',24,True)
text(82,325,'대면활동 의존 · 업종구조와 다양성',24)
text(82,361,'고객 구성 · 수요 기반',24)
text(82,412,'대응력',24,True)
text(82,449,'상인조직의 공동 대응 · 자금 조달 가능성',24)
text(82,485,'기존 채무 등 금융 제약',24)
text(742,239,'외부 연대성',29,True)
text(742,288,'외부 자원 접근',24,True)
text(742,325,'자금·정보·서비스 접근',24)
text(742,361,'지원사업 참여·수혜 이력',24)
text(742,412,'공동 논의·협력',24,True)
text(742,449,'외부 기관과의 협의 참여',24)
text(742,485,'공동사업과 실행 활동',24)
merge(528,570)
rect(220,570,960,74,'#F5F7F9')
text(700,581,'상권별·시기별 자료 결합',25,True,center=True)
text(700,614,'경계·코드·시점 확인  /  내부 조직과 외부 관계 구분',22,fill=MUTED,center=True)
line([(700,644),(700,674)],True)

header(681,2,'내부 특성 × 외부 연대성의 관계 분석과 미래 예측')
rect(55,753,1290,84,BLUE)
text(700,765,'외부 연대성과 이후 경제적 성과의 관계는',27,True,center=True)
text(700,800,'상권의 민감성과 대응력에 따라 어떻게 달라지는가?',27,True,center=True)
rect(55,858,630,148)
rect(715,858,630,148)
text(82,877,'H5a  보완 가설',26,True)
text(82,920,'민감성이 높거나 대응 자원이 부족할 때',24)
text(82,956,'연대성과 양호한 성과의 관계가 더 뚜렷한가?',24)
text(742,877,'H5b  강화 가설',26,True)
text(742,920,'내부 역량이 갖추어진 상권에서',24)
text(742,956,'연대성과 양호한 성과의 관계가 더 뚜렷한가?',24)
rect(55,1030,630,175,'#F5F7F9')
rect(715,1030,630,175,'#F5F7F9')
text(82,1049,'관계 분석  |  패널 회귀',26,True)
text(82,1092,'H1~H3  업종구조·수요의 기본 관계와 차이',23)
text(82,1129,'H5a·H5b  내부 조건 × 외부 연대성',23)
text(82,1167,'전체 분석 + 자료 확보 상권의 심화 분석',21,fill=MUTED)
text(742,1049,'미래 예측  |  회귀·트리 기반 모형',26,True)
text(742,1092,'H4  업종구조·수요 정보의 추가 예측 가치',23)
text(742,1129,'H6  조직·연대성 정보의 추가 예측 가치',23)
text(742,1167,'이후 분기 폐업률·명목 매출 변화 / 시간외 평가',21,fill=MUTED)
merge(1205,1249)

header(1255,3,'양적·질적 결과의 종합과 리질리언스 해석')
rect(55,1330,630,156,BLUE)
rect(715,1330,630,156,TEAL)
text(82,1349,'양적 결과  |  경제활동의 규모와 변화',26,True)
text(82,1396,'매출·거래 변화',24)
text(82,1435,'점포 수 · 개업·폐업',24)
text(742,1349,'질적 결과  |  업종 구성과 상권 기능',26,True)
text(742,1396,'업종별 구성비 · 업종의 진입·퇴출과 재편',24)
text(742,1435,'생활·소비 기능의 변화',24)
merge(1486,1530)
rect(55,1530,1290,113,BLUE)
text(700,1548,'지역사회 경제 리질리언스의 종합 해석',28,True,center=True)
text(700,1596,'유지·개선과 재편의 조합  ·  취약성의 양상  ·  연대성의 보완·강화 조건',24,center=True)
line([(700,1643),(700,1680)],True)
rect(55,1680,1290,109,TEAL)
text(700,1696,'과정 기록을 통한 적응적 거버넌스 탐색',27,True,center=True)
text(700,1741,'공동 평가·학습 → 지원 방식·공동 대응의 수정 여부 확인',24,center=True)
text(55,1824,'주  화살표는 연구 수행 순서이며 인과경로나 현상의 발생 순서를 뜻하지 않음.',21,fill=MUTED)
text(55,1857,'조직·금융·연대성 항목은 자료 확보 범위에서 분석하며, 업종 재편을 곧바로 개선으로 판단하지 않음.',21,fill=MUTED)
text(55,1895,'자료: 연구계획서 제2.1~2.6절에 기초한 연구자 작성.',19,fill=MUTED)

im.save(OUT/'research-framework.png',dpi=(300,300))
svg.append('</svg>')
(OUT/'research-framework.svg').write_text('\n'.join(svg),encoding='utf-8')
im.resize((980,1358),Image.Resampling.LANCZOS).save(OUT/'research-framework-preview.png')
print(f'PNG: {im.size}, 300 dpi; SVG: {W} x {H}')
