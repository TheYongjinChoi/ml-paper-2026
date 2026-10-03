# =============================================================================
# 00_02_project_rules.R
# Project coding and data-management rules
# 실행용 코드가 아니라 프로젝트 규칙 기록용 파일
# =============================================================================
#
# [Core rules]
#
# 1. script_name   = 실제 R script 이름(.R 제외)
# 2. script_prefix = 저장파일 공통 prefix
#
# 3. data/processed/<script_name>/
#
# 4. output/<script_name>/
#      ├─ figures/
#      └─ tables/
#
# 5. 파일명
#    <script_prefix>_<산출물 내용>.<확장자>
#
# 6. Data
#    - RDS 기본
#    - 최종 분석·Stata 검산·공유가 필요한 핵심자료만 DTA 추가
#
# 7. Tables
#    - CSV 기본
#    - 논문 핵심표: CSV + TEX
#
# 8. Figures
#    - PNG + PDF
#
# 9. Interpretation / Conclusion
#    - TXT
#
# 10. View()
#     if (interactive()) View(...)
#
# 11. 분석 script에서는 dir.create() 반복 사용하지 않음
#
# 12. 주요 분석 script 상단에는 Workflow 유지
#
# =============================================================================