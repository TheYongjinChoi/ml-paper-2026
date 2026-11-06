# panel_data_21-25

- 파일: `panel_data_21-25.csv`
- 코드북: `panel_data_21-25_codebook.csv`
- 단위: 서울 전통시장 상권 × 분기, 305개 시장 × 20분기 = 6,100행
- 인코딩: UTF-8 BOM. 현재 Windows R 환경에서는 `readr::read_csv("panel_data_21-25.csv", locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE)`로 읽는다. 기본 `read.csv()`는 현재 시스템 로캘에서 한글 값을 잘못 읽을 수 있다.
- 열: 원본 `data.csv` 319개 열과 상인조직·지원사업 변수 14개 열, 총 333개. 모든 열 이름은 영문 소문자 `snake_case`이며 중복이 없다. 코드북의 `original_column`에서 원본 한글 열 이름을 확인할 수 있다. 원본 값은 변경하지 않았다.
- 연결 키: `code`와 `period`. 원본의 `name`, `year`, `quarter`와 결합 자료의 값도 대조하였다. 6,100행 모두 연결되었고 키 중복은 없다.
- `organization_presence_2020_0_1`: 1=2020년 자료에 조직 형태가 법적·임의단체, 0=시장 기록은 있으나 조직 형태와 상인회명이 모두 공란, 공란=시장명 대응 또는 조직 존재 확인 불가.
- `support_active_0_1`: 1=해당 연도가 공개된 사업의 `지원기간`에 포함, 0=그 외. `support_stage_observed`에 더 상세한 상태를 보존한다.
- `support_application_status`, `support_withdrawal_status`: 신청·포기 자료가 없어 `자료없음`. 지원 중 여부에 따라 추정하지 않았다.
- 지원 상태는 연도 단위여서 같은 연도의 네 분기에 같은 값이 반복된다. 분기별 개시·종료일은 확인되지 않았다.
- 사용자 검토표의 시장명 조정 중 충돌 없이 확인된 대응만 반영했다. 남은 보류 사례는 `04_analysis/support_programs/review_decisions_recovered_korean.csv`를 참조한다.
