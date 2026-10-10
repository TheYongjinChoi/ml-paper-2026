from pathlib import Path
import json, csv, re, shutil
from collections import defaultdict

ROOT=Path(__file__).resolve().parents[1]
WORK=ROOT/'codebook'
OUT=Path(r'C:\Users\chwou\Desktop\graduate\03_data\processed\서울시상권_코드북_2021_2025')
OUT.mkdir(exist_ok=True)
inv=json.loads((WORK/'source_inventory.json').read_text(encoding='utf-8'))
industries=json.loads((WORK/'industry_codes.json').read_text(encoding='utf-8'))
rows=[]

def add(code,name,role,component,group,formula,fields,unit='비율',scope='주분석',status='산출 가능·비교성 검증 필요',lag='t−1 → t',meaning='',caution='',source='T1; D1',missing='원자료 결측은 NA. 분모 0은 NA. 미관측 업종을 임의로 0 처리하지 않음.'):
    rows.append(dict(구분=role,구성요소=component,특성군=group,변수=name,변수코드=code,분석범위=scope,조작적정의_산식=formula,원자료_열=fields,단위=unit,시간배치=lag,채택상태=status,개념연결_예상관계=meaning,결측_처리=missing,해석상주의=caution,근거=source))

N='점포: 유사_업종_점포_수'
K='상권×분기×서비스_업종_코드'
S='추정매출: 당월_매출_금액'
for code,name,field,rule in [
 ('district_id','상권 식별자','상권_코드 / trdar_cd','문자형으로 보존. 주분석 상권×분기 키'),
 ('quarter','기준 분기','기준_년분기_코드 / stdr_yyqu_cd','20211~20254. q_index=4×연도+분기−1; 시차는 연속 분기 기준'),
 ('industry_code','생활밀접업종 코드','서비스_업종_코드 / svc_induty_cd','100개 코드 고정 목록. 통합 전 상권×분기×업종 유일성 확인'),
 ('hinterland_id','배후지 식별자','상권배후지_코드 / trdar_cd','골목상권 코드와 일치 여부 및 공간 범위 확인 후 연결')]:
    add(code,name,'식별자','해당 없음','분석 키',rule,field,'문자',scope='공통' if code!='hinterland_id' else '배후지 보조',status='보유',lag='해당 시점',source='D1; F01~F35',missing='결측 식별자는 결합 제외 및 별도 보고')

# Outcomes are kept separate from the characteristics used to explain them.
outcomes=[
 ('sales_total','명목 추정매출액','경제활동 규모','Σk S_ikt',S,'원','성과의 수준. 직접적인 영업이익이 아님'),
 ('sales_yoy','매출 변화율','유지·개선','100×ln(S_it/S_i,t−4)',S,'100×로그비','2022~2025 산출. 같은 업종 포괄 범위 확인'),
 ('transactions_yoy','거래 건수 변화율','유지·개선','100×ln(C_it/C_i,t−4)','추정매출: 당월_매출_건수','100×로그비','2022~2025. 매출과 함께 수량 측면 해석'),
 ('stores_total','전체 점포 수','경제활동 규모','N_it=Σk 유사_업종_점포_수',N,'개','당기 폐업 포함. 현재 운영 중인 점포만의 재고가 아님'),
 ('stores_yoy','점포 수 변화율','유지·개선','100×ln(N_it/N_i,t−4)',N,'100×로그비','2022~2025. 일반점포+프랜차이즈 포함 범위 통일'),
 ('closure_rate','폐업률','유지·개선','100×Σk 폐업_점포_수 / N_it','점포: 폐업_점포_수; 유사_업종_점포_수','%','업종별 제공률 단순평균 금지. 코호트 생존율 아님'),
 ('opening_rate','개업률','진입·퇴출','100×Σk 개업_점포_수 / N_it','점포: 개업_점포_수; 유사_업종_점포_수','%','제공 개업률과 별도 정합성 검증'),
 ('replacement_ratio','개폐업 대체율','진입·퇴출','100×Σk 개업_점포_수 / Σk 폐업_점포_수','점포: 개업_점포_수; 폐업_점포_수','%','폐업 0이면 NA. 개업·폐업 수 함께 제시'),
 ('industry_reallocation','업종 구성 변화량','질적 재편','0.5×Σk |p_ikt−p_ik,t−4|; p_ikt=N_ikt/N_it',N+'; 서비스_업종_코드','0~1','2022~2025. 변화가 크다는 사실은 개선을 의미하지 않음'),
 ('industry_share_change_k','업종별 비중 변화','질적 재편','100×(p_ikt−p_ik,t−4), k별로 산출',N+'; 서비스_업종_코드','%p','변수군. 모든 업종 비중을 설명모형에 동시에 넣지 않음'),
 ('function_share_change_g','기능군별 비중 변화','질적 재편','100×(Σk∈g p_ikt−Σk∈g p_ik,t−4)',N+'; 기능군 대응표(추가 확정)','%p','기능군별 절대 점포 수 변화와 병기; 기능군 코드표 확정 후 산출'),
 ('function_count_change_g','기능군별 점포 수 변화','질적 재편','Σk∈g N_ikt−Σk∈g N_ik,t−4',N+'; 기능군 대응표(추가 확정)','개','기능군 변화는 동일 점포의 업종 전환을 뜻하지 않음')]
for code,name,group,formula,field,unit,caution in outcomes:
    add(code,name,'종속변수','성과: 적응성 관련' if group=='질적 재편' else '경제적 성과',group,formula,field,unit,lag='t의 결과',meaning='설명요인과 분리해 결과로 관측',caution=caution,source='D1; T2; T3; T7',status='분류표 확정 후 산출' if 'function_' in code else '산출 가능·비교성 검증 필요',missing='비교 두 시점 중 결측이면 NA. 로그는 양수만. 폐업 0의 대체율은 NA.')
add('sales_drawdown','관측기간 내 매출 하락폭','보조 결과','성과: 내구성 관련','유지·개선','사전 지정 비교시점 S_base 대비 100×(S_it/S_base−1)',S,'%',lag='사건·기준시점 별도 정의',status='보조 결과 후보',meaning='관측기간 내 기능 유지의 결과적 측면',caution='2021은 이미 코로나19 이후. 초기 충격 저항력으로 해석하지 않음',source='T1; T2; D1')
add('rebound_quarters','관측기간 내 반등 소요 분기','보조 결과','성과: 신속성 관련','반등 속도','사전 정의한 하락 사건 후 지정 기준을 다시 충족하기까지 분기 수',S,'분기',lag='사건 이후',status='사건·반등 기준 확정 전 보류',meaning='결과적 반등 속도',caution='미반등은 우측검열. 임의 최대값·0 대체 금지. 설명변수로 동시 투입 금지',source='T1; T2; D1')
add('commercial_change_class','제공 상권변화지표','보조 결과','경제적 성과의 기술 지표','영업기간 구성','상권_변화_지표 및 상권_변화_지표_명 원값 보존','상권변화지표: 상권_변화_지표; 상권_변화_지표_명','범주',lag='t의 기술 결과',status='기술통계 보조',meaning='제공기관이 영업기간을 이용해 분류한 상권 상태',caution='독자적인 종합 리질리언스·취약성 등급이나 개별 점포 생존확률로 해석하지 않음. 운영 평균 영업기간과 중복 고려',source='D1; 기존 코드북')

# Robustness is a theoretical connection, not a validated direct scale.
add('restricted_industry_share','제한 대상 업종 의존도','설명변수','내구성','민감성 관련 내부 조건','Σk∈정책제한업종 N_ik,t−1 / N_i,t−1',N+'; 방역조치별 업종·기간 대응표','0~1',status='정책·업종 대응표 추가 필요',meaning='대면활동 제한에 민감한 구조의 후보. 성과와 음의 관계 예상',caution='정책별 기간과 업종 매칭 필요. 외식업 전체를 제한 업종으로 단정하지 않음',source='T1; T3')
add('resident_cv4','상주 수요 변동성','설명변수','내구성','수요 안정성 후보','sd(P_i,t−4:t−1)/mean(P_i,t−4:t−1); 표본표준편차','상주인구: 총_상주인구_수','비율',lag='과거 4분기 → t',status='보조 후보',meaning='지속적인 수요 기반의 불안정성 대리. 낮을수록 안정',caution='2022Q1부터. 계절성·추계 수정과 구분. 주민의 소비행동이나 실제 내구성 직접 측정 아님',source='T1; D1',missing='4개 연속 분기 완비, 평균>0일 때만 산출')
add('operating_age','운영 점포 평균 영업기간','설명변수','내구성','기존 영업 지속성 후보','운영_영업_개월_평균 원값','상권변화지표: 운영_영업_개월_평균','개월',status='보조 후보',meaning='기존 사업 기반의 지속성에 관한 대리',caution='생존자 구성 편향. 개별 생존시간·충격 저항력과 다름. 고정효과와 과거 성과 중복 검토',source='T1; D1 p.3')

for code,name,formula,unit,caution in [
 ('shannon','업종 다양성','−Σk p_k ln(p_k)','nat','주 다양성 지표. p=0 항은 0. 관측 누락과 진짜 0 구분'),
 ('inverse_hhi','유효 업종 수','1 / Σk p_k²','유효 업종 수','기존 0_codebook의 HHI 역수. Shannon과 대안 모형에서 비교'),
 ('hhi','업종 집중도','Σk p_k²','0~1','높을수록 집중. 역수·Shannon과 동일 모형에 중복 투입 지양'),
 ('industry_richness','영업 업종 수','Σk 1[N_k>0]','업종 수','규모에 민감. 전체 점포 수를 고려한 보조 지표'),
 ('function_backup_share','기능별 복수 공급 비중','Σg 1[N_g≥2] / Σg 1[N_g>0]','0~1','기능군 확정 필요. 2개는 최소 복수 공급의 조작적 기준이며 실제 유휴역량 아님')]:
    add(code,name,'설명변수','가외성','대체·분산 조건',formula+'; p_k=N_k/Σk N_k',N+'; 서비스_업종_코드'+('; 기능군 대응표' if code=='function_backup_share' else ''),unit,meaning='충격 분산 또는 기능 대체 가능성의 대리',caution=caution+'; 다양성이 높으면 반드시 성과가 좋다는 뜻은 아님',status='분류표 확정 후 산출' if code=='function_backup_share' else '산출 가능·비교성 검증 필요',source='T1 pp.431–433; T3')

for code,name,family,field in [('resident_pop','상주인구','상주인구','총_상주인구_수'),('worker_pop','직장인구','직장인구','총_직장_인구_수'),('footfall','길단위 유동인구','길단위인구','총_유동인구_수')]:
    add(code,name,'설명변수','자원부존성','경제적 수요 기반',f'원값 {field}; 모형에서는 ln(1+x)',family+': '+field,'명 또는 제공 추계 인구량',meaning='상권이 활용할 수 있는 잠재 수요의 대리',caution='실제 동원·구매를 직접 측정하지 않음. 세 인구를 합산하거나 합계로 비중화하지 않음. 유동인구와 길단위인구는 같은 변수',source='T1; T3; T4; D1 pp.4–5')
for code,name,formula,fields,unit,caution in [
 ('org_participation','상인조직 참여율','활동 참여 점포 수 / 같은 범위의 조직 참여 대상 운영 점포 수','상인조직 명부·당시 활동·대상 점포 수','0~1','상인회 존재만으로 활동 능력을 추론하지 않음'),
 ('org_joint_execution','내부 공동사업 실행 건수','직전 1년 내 내부 공동사업 실제 실행 건수','상인조직 실행보고·활동 일자','건','외부 공동사업 수와 참여 범위 구분. 동일 건수 이중 투입 금지'),
 ('org_coordination','내부 의사결정 활동','직전 1년 내 안건·결정·실행이 확인된 내부 회의 수','내부 회의록·실행 기록','건','회의 횟수는 질이나 민주성의 직접 측정 아님'),
 ('finance_headroom','추가 자금조달 여력','확인된 추가 이용 가능 한도 / 대상 사업체 수','비식별 사업체별 한도·기사용액·대상 모집단','원/사업체','현재 대출액·은행 수를 여력으로 대체하지 않음'),
 ('finance_new_supply','신규 보증·대출 공급','기간 신규 공급액 / 같은 범위 대상 사업체 수','재단·금융기관 비식별 신규 공급·사업체 수','원/사업체','공급은 실제 이용. 잠재 여력과 구분; 보증을 전체 대출로 해석하지 않음'),
 ('finance_user_share','금융 이용 사업체 비율','기간 이용 사업체 수 / 같은 범위 대상 사업체 수','보증·대출 이용 명부의 비식별 집계','0~1','금융기관 이용 자체로 연대성 인정 금지'),
 ('finance_debt_burden','기존 금융 부담','기초 채무 잔액 / 동일 범위 과거 연간 매출','비식별 채무 잔액·동일 사업체 매출','비율','채무·매출 포괄 사업체 일치 필요. 당기 지원의 사후 통제 금지')]:
    add(code,name,'설명변수','자원부존성','조직적 역량' if code.startswith('org') else '금융적 대응 자원',formula,fields,unit,scope='심화 분석',status='추가 자료 필요',lag='성과 이전 확인 시점; 연간이면 연간 심화',meaning='자원의 식별·동원·활용 능력. 금융 부담은 제약 측면',caution=caution,source='T1; T5; T6; 연구자 조작화',missing='미확보·미상은 NA. 활동·이용 없음이 확인된 경우만 0')

add('org_learning','공동 학습·훈련 참여율','설명변수','적응성','조직적 학습','이전 기간 학습·훈련 참여 점포 수 / 동일 대상 점포 수','상인조직 교육·훈련 명부 및 기간','0~1',scope='심화 분석',status='추가 자료 필요',meaning='학습 능력의 투입·활동 측면',caution='참여가 실제 학습이나 성공적 적응을 입증하지 않음',source='T1; T5; 연구자 조작화')
add('response_revision','평가에 근거한 대응 수정','과정변수','적응성','적응적 거버넌스','평가 결과와 변경 결정·시점이 연결된 대응 수정 기록 유무(1/0)','평가서·회의록·사업 변경 전후 기록','0/1',scope='심화·문서 분석',status='추가 자료 필요',lag='의사결정·실행 시점 보존',meaning='학습·평가와 대응 조정 과정',caution='회의 개최만으로 1 부여 금지. 확정된 매개변수로 사용하지 않음',source='T1 pp.444–445; 연구자 조작화',missing='전체 기록 접근 가능하고 해당 활동 없음 확인 시 0; 기록 부재는 NA')
add('response_delay_days','공동 대응 착수 지연','설명변수','신속성','조직적 대응 속도','실제 공동 대응 착수일 − 비교 가능한 사건·요청일','사건일·요청일·결정일·실행일 기록','일',scope='심화 분석',status='추가 자료 필요',lag='사건별; 이후 성과와 연결',meaning='짧을수록 빠른 착수',caution='동일 사건·업무 범위 비교. 단순 회의일은 실행일이 아님. 당기 결과의 사후 정보로 예측 금지',source='T1; 연구자 조작화')
for code,name,formula,fields,unit,caution in [
 ('external_access','외부 지원·서비스 접근 여부','이전 기간 실제 이용·수혜 확인 시 1','지원사업 대상·선정일·실행·수혜 기록','0/1','일방적 지원 수혜만으로 공동 협력 인정 금지'),
 ('external_support_intensity','외부 지원 강도','실제 집행액 / 동일 범위 지원 대상 점포 수','지원사업 실행액·대상 범위·기간','원/대상 점포','지원대상 선정·지원 필요성의 역인과 고려'),
 ('external_joint_meetings','외부 공동 논의 참여','상권 대표의 실제 참석과 공동 안건이 확인된 회의 수','협의체 회의록·참석자·안건','건','자치구 협의체 존재를 모든 상권의 참여로 전환하지 않음'),
 ('external_joint_projects','외부 공동사업 실행','외부 기관과 함께 실행한 사업 수','기관·상권 참여 및 실행 이력','건','내부 사업과 중복 건수 분리'),
 ('external_partner_count','실제 협력 기관 수','해당 기간 공동 활동이 확인된 고유 외부 기관 수','참여 기관 명부·공동 활동','개','인근 대학·은행 수가 협력 기관 수는 아님'),
 ('external_other_district_ties','다른 상권과의 협력 연결 수','공동 활동 기록이 확인된 다른 상권의 고유 수','상권 간 공동사업·연결 이력','개','지리적 인접·업종 유사성을 사회적 연대성으로 대체하지 않음')]:
    add(code,name,'설명변수','연대성','외부 자원 접근' if code in ['external_access','external_support_intensity'] else '공동 논의·협력',formula,fields,unit,scope='심화 분석',status='추가 자료 필요',lag='성과 이전 기록; 실제 기간 보존',meaning='상권과 다른 시스템 사이의 관계를 관측',caution=caution,source='T1 pp.433–434; 연구자 조작화',missing='비참여·미수혜 확인 시 0; 명부 누락·기록 미확보는 NA')
add('network_betweenness','외부자원 매개성','설명변수','연대성','연결망 구조','정의된 전체 연결망에서 매개중심성 산출','전체 노드·관계·기간·방향성 자료','정의에 따름',scope='심화 분석',status='연결망 전수·포괄성 확보 전 보류',meaning='자원 연결의 매개 위치 후보',caution='일부 기관 명부만으로 지수 계산하지 않음. 기준 연결망 없으면 채택하지 않음',source='기존 코드북 후보; 연구자 조작화')

# Controls: distinguish context from a validated resilience component.
controls=[
 ('district_type','상권 유형','A=골목상권, D=발달상권, R=전통시장; U 제외','상권_구분_코드','범주','상권 고정효과에 주효과 흡수. 유형×시기 또는 층화에 활용'),
 ('district_area','상권 면적','영역_면적 / 1000000','영역: 영역_면적','km²','m² 원단위 및 경계시점 확인. 불변이면 상권 고정효과에 흡수'),
 ('store_density','점포 밀도','N_it / area_i',N+'; 영역: 영역_면적','개/km²','집적·경쟁 통제. 규모·면적과 중복·공선성 검토'),
 ('franchise_share','프랜차이즈 비중','Σk 프랜차이즈_점포_수 / N_it','점포: 프랜차이즈_점포_수; 유사_업종_점포_수','0~1','본사 자원·조직력·연대성의 직접 척도로 간주하지 않음'),
 ('food_share','외식업 비중','Σk∈CS1 N_ikt / N_it',N+'; 서비스_업종_코드','0~1','산업구조 통제. 제한업종 비중과 다른 변수'),
 ('service_share','서비스업 비중','Σk∈CS2 N_ikt / N_it',N+'; 서비스_업종_코드','0~1','소매를 기준범주로 제외하는 등의 구성비 제약 적용'),
 ('retail_share','소매업 비중','Σk∈CS3 N_ikt / N_it',N+'; 서비스_업종_코드','0~1','외식·서비스·소매 전체를 절편과 동시 투입하지 않음'),
 ('footfall_weekend_share','주말 유동인구 비중','(토요일_유동인구_수+일요일_유동인구_수)/총_유동인구_수','길단위인구: 총_유동인구_수; 토요일_유동인구_수; 일요일_유동인구_수','0~1','제공 총계와 요일합 정합성 확인. 방문 목적을 직접 측정하지 않음'),
 ('footfall_senior_share','60세 이상 유동인구 비중','연령대_60_이상_유동인구_수/총_유동인구_수','길단위인구: 연령대_60_이상_유동인구_수; 총_유동인구_수','0~1','고령 고객의 취약성을 자동 전제하지 않음'),
 ('footfall_evening_share','저녁 유동인구 비중','(시간대_17_21_유동인구_수+시간대_21_24_유동인구_수)/총_유동인구_수','길단위인구: 시간대_17_21_유동인구_수; 시간대_21_24_유동인구_수; 총_유동인구_수','0~1','시간대합 정합성 확인; 심야 제한 노출과 동일시하지 않음'),
 ('subway_count','지하철역 수','지하철_역_수','집객시설: 지하철_역_수','개','시설 존재와 실제 접근 거리·수송 역량을 구분'),
 ('bus_stop_count','버스 정거장 수','버스_정거장_수','집객시설: 버스_정거장_수','개','주기적 갱신·과거 값 유효성 확인 전 주분석 제외'),
 ('attractor_count','집객시설 수','집객시설_수','집객시설: 집객시설_수','개','주어진 총계 사용. 시설 세부 합산은 중복·포괄 범위 확인'),
 ('bank_count','은행 수','은행_수','집객시설: 은행_수','개','과거 불변·빈칸 의미 미확인. 신용여력·연대성 대리로 사용 금지'),
 ('apartment_price','아파트 평균 시가','아파트_평균_시가','아파트: 아파트_평균_시가','원단위 확인 필요','상가 임대료·사업체 자산으로 해석하지 않음'),
 ('consumption_total','제공 소비 지출 총액','지출_총금액','소비: 지출_총금액','원단위·모집단 확인 필요','소득·소비여력·상권 매출과 동일시하지 않음'),
 ('gu_code','자치구','자치구 코드 범주','영역: 자치구_코드','범주','상권 고정효과와 중복. 지역×시기 충격 등 필요성 있을 때만'),
 ('dong_code','행정동','행정동 코드 범주','영역: 행정동_코드','범주','공간 경계와 코드 기준시점 확인. 상권을 행정동으로 대체하지 않음'),
 ('quarter_fe','분기 고정효과','20개 분기 범주 중 기준범주 제외','기준_년분기_코드','범주','공통 충격·계절성 통제; 연도·계절 더미와 중복 투입 금지'),
 ('past_sales','과거 매출 실적','예측 시점에 공개된 최근 매출 수준·변화',S,'원·변화율','관계 분석에 시차 종속변수 자동 추가 금지. 예측 기준모형 정보'),
 ('past_closure','과거 폐업 실적','예측 시점에 공개된 최근 폐업률','점포: 폐업_점포_수; 유사_업종_점포_수','%','모형 평가자료의 미래 값을 보간에 사용하지 않음'),
 ('rent_level','상가 임대료 수준','동일 기준 면적당 실질 또는 명목 월 임대료','추가 임대료 조사·대상·기간','원/m²/월','아파트 시가로 대체 금지. 보증고객 표본 편향 고려')]
for code,name,formula,fields,unit,caution in controls:
    state='추가 자료 필요' if code=='rent_level' else ('투입 보류·정의/시점 검증 필요' if code in ['bank_count','subway_count','bus_stop_count','attractor_count','apartment_price','consumption_total'] else '산출 가능·비교성 검증 필요')
    add(code,name,'통제변수' if not code.startswith('past_') else '예측 통제','구성요소 외','입지·규모·구성·시기',formula,fields,unit,status=state,lag='시간 고정/해당 분기' if code in ['district_type','district_area','gu_code','dong_code','quarter_fe'] else 't−1 → t',meaning='구성요소의 직접 척도에 해당하지 않는 혼란·맥락 조건',caution=caution,source='D1; T3; T4; 기존 코드북 검토')

# Auxiliary hinterland variables remain attached to the focal commercial district outcome.
bh=[
 ('bh_shannon','배후지 업종 다양성','설명변수','가외성','−Σk p_Hk ln(p_Hk)','점포 배후지: 유사_업종_점포_수; 서비스_업종_코드','nat','대체·분산 조건','실제 가외역량과 구분. 경쟁 가능성도 함께 검토'),
 ('bh_function_backup','배후지 동일 기능 대체 공급','설명변수','가외성','Σg p_Cg×1[N_Hg>0]','점포 상권·배후지: 유사_업종_점포_수; 기능군 대응표','0~1','상권 기능의 외부 대체 가능성','기능군 확정·상권 자체 포함 여부 검증 필요'),
 ('bh_complement_share','배후지 보완 업종 비중','설명변수','가외성','Σk 1[N_Ck=0]×N_Hk / N_H','점포 상권·배후지: 유사_업종_점포_수; 서비스_업종_코드','0~1','상권에 없는 기능·업종의 주변 공급 후보','미관측 업종을 0으로 해석할 수 있을 때만. 업종 상보성과 동일하지 않음'),
 ('bh_resident_cv4','배후지 상주 수요 변동성','설명변수','내구성','sd(P_H,t−4:t−1)/mean(P_H,t−4:t−1)','상주인구 배후지: 총_상주인구_수','비율','주변 수요의 안정성과 상권의 유지 관계','낮을수록 안정. 계절성·자료 수정 검토'),
 ('bh_resident_pop','배후지 상주인구','설명변수','자원부존성','ln(1+총_상주인구_수)','상주인구 배후지: 총_상주인구_수','로그 인구','주변 수요 자원','주분석 내부 수요와 별도 공간 범위로 구분'),
 ('bh_worker_pop','배후지 직장인구','설명변수','자원부존성','ln(1+총_직장_인구_수)','직장인구 배후지: 총_직장_인구_수','로그 인구','주변 근로 수요 자원','상주인구와 합산 금지'),
 ('bh_footfall','배후지 길단위 유동인구','설명변수','자원부존성','ln(1+총_유동인구_수)','길단위인구 배후지: 총_유동인구_수','로그 추계량','주변 이동·방문 수요 자원','상권·배후지의 합산 방문자 수를 만들지 않음'),
 ('bh_competitor_supply','배후지 동일 업종 공급 압력','보조 설명·통제','구성요소 외: 경쟁 조건','Σk p_Ck×N_Hk','점포 상권·배후지: 유사_업종_점포_수; 서비스_업종_코드','가중 점포 수','경쟁성 가설의 설명요인','인접 동일업종은 집적 이익도 가능. 경쟁의 인과효과로 단정 금지'),
 ('bh_composition_overlap','상권·배후지 업종 구성 유사도','보조 설명·통제','구성요소 외: 경쟁 조건','Σk min(p_Ck,p_Hk)','점포 상권·배후지: 유사_업종_점포_수; 서비스_업종_코드','0~1','유사한 공급 구조의 정도','가외성과 경쟁성의 상반된 해석을 비교; 한 방향의 질적 판단 금지'),
 ('bh_store_density','배후지 점포 밀도','통제변수','구성요소 외','N_H / area_H','점포 배후지: 유사_업종_점포_수; 영역 배후지: 영역_면적','개/km²','주변 집적 규모 통제','영역 1071개·통계 1090개 코드 불일치. 결합 가능한 표본만'),
 ('bh_area','배후지 면적','통제변수','구성요소 외','영역_면적 / 1000000','영역 배후지: 영역_면적','km²','주변 범위 통제','배후지 형상 시점·상권 포함·중첩 검증 필요'),
 ('bh_sales_yoy','배후지 매출 변화','보조 결과','경제적 성과','100×ln(S_Ht/S_H,t−4)','추정매출 배후지: 당월_매출_금액','100×로그비','상권과 주변의 변화 경로 비교','당기 배후지 결과를 사전 대응력으로 사용하지 않음')]
for code,name,role,comp,formula,fields,unit,meaning,caution in bh:
    add(code,name,role,comp,'배후지 보조분석',formula,fields,unit,scope='골목상권 배후지 보조',status='공간·모집단 확인 후 산출',lag='t의 결과' if code=='bh_sales_yoy' else ('과거 4분기 → t' if code=='bh_resident_cv4' else 't−1 → t'),meaning=meaning,caution=caution+'; 전통시장·발달상권에 대입하지 않음',source='T1; 연구자 조작화; D1; D5')

for code,name,formula,caution in [
 ('sensitivity_x_solidarity','민감성×연대성','사전 지정 민감성 지표 × 외부 연대성 지표','H5a 보완 후보. 총합 취약성 점수 사용 금지'),
 ('capacity_x_solidarity','대응 자원×연대성','사전 지정 조직·금융 자원 지표 × 외부 연대성 지표','H5a/H5b. 부족한 자원과 풍부한 역량을 조건별 해석')]:
    add(code,name,'상호작용','구성요소 간 결합','내부·외부 결합',formula,'본 코드북의 채택된 내부 조건·연대성 변수','곱의 단위',scope='심화 분석',status='원변수 확보 후',meaning='단일 개념이나 매개변수 아님',caution=caution+'; 각 주효과 포함. 연속변수 중심화는 훈련 자료 기준',source='연구가설 H5a·H5b',missing='한 원변수라도 미상일 때 NA')

# Preserve exact raw columns and make all English aliases explicit.
alias={'stdr_yyqu_cd':'기준_년분기_코드','trdar_se_cd':'상권_구분_코드','trdar_se_cd_nm':'상권_구분_코드_명','trdar_cd':'상권_코드','trdar_cd_nm':'상권_코드_명','svc_induty_cd':'서비스_업종_코드','svc_induty_cd_nm':'서비스_업종_코드_명','stor_co':'점포_수','similr_induty_stor_co':'유사_업종_점포_수','opbiz_rt':'개업_율','opbiz_stor_co':'개업_점포_수','clsbiz_rt':'폐업_률','clsbiz_stor_co':'폐업_점포_수','frc_stor_co':'프랜차이즈_점포_수'}
prefixes={'thsmon':'당월','mdwk':'주중','wkend':'주말','mon':'월요일','tues':'화요일','wed':'수요일','thur':'목요일','fri':'금요일','sat':'토요일','sun':'일요일','ml':'남성','fml':'여성'}
for a,b in prefixes.items():
    for suf,ko in [('amt','금액'),('co','건수')]:alias[f'{a}_selng_{suf}']=f'{b}_매출_{ko}'
for band in ['00_06','06_11','11_14','14_17','17_21','21_24']:
    for suf,ko in [('amt','금액'),('co','건수')]:alias[f'tmzon_{band}_selng_{suf}']=f'시간대_{band.replace("_","~")}_매출_{ko}'
for age in ['10','20','30','40','50','60_above']:
    for suf,ko in [('amt','금액'),('co','건수')]:alias[f'agrde_{age}_selng_{suf}']=f'연령대_{age.replace("above","이상")}_매출_{ko}'
def canon(c,scope):
    x=alias.get(c.lower(),c)
    if x.startswith('시간대_건수~'):
        end=x.split('~')[1][:2]; start={'06':'00','11':'06','14':'11','17':'14','21':'17','24':'21'}[end]
        x=f'시간대_{start}~{end}_매출_건수'
    if scope=='배후지':
        if x in ['상권_코드','상권배후지_구분_코드']:x='상권배후지_코드'
        if x in ['상권_코드_명','상권배후지_구분_코드_명']:x='상권배후지_코드_명'
    return x

rawgroups=defaultdict(lambda:{'files':[],'names':set(),'periods':set()})
file_rows=[]
for i,f in enumerate(inv,1):
    fid=f'F{i:02}'
    for c in f['columns']:
        k=(f['family'],f['scope'],canon(c,f['scope']))
        rawgroups[k]['files'].append(fid);rawgroups[k]['names'].add(c);rawgroups[k]['periods'].update(f['periods'])
    file_rows.append(dict(자료ID=fid,자료군=f['family'],공간=f['scope'],파일명=f['file'],인코딩=f['encoding'],전체행=f['rows'],기간내행=f['target_rows'],대상행=f['main_rows'],대상상권수=f['districts'],관측분기수=len(f['periods']),최초분기=min(f['periods']) if f['periods'] else '시점 없음',최종분기=max(f['periods']) if f['periods'] else '시점 없음',키중복=f['duplicate_keys'],유형=json.dumps(f['type_counts'],ensure_ascii=False),원본경로=f['path']))
raw_rows=[]
for (fam,scope,c),v in sorted(rawgroups.items()):
    note='열 이름은 원문 보존. 의미·단위는 제공정보와 함께 검토'
    unit='원자료 정의 확인'
    if '코드' in c or '지표' in c:unit='문자/범주'
    elif '개월' in c:unit='개월'
    elif c in ['개업_율','폐업_률']:unit='%'
    elif '매출_금액' in c:unit='원(명목 추정액)'
    elif '매출_건수' in c:unit='건'
    elif '인구_수' in c:unit='명/제공 추계량; 실제 방문자 고유 수 아님'
    elif c=='영역_면적':unit='m²; 좌표·경계 확인'
    elif c.endswith('_수'):unit='개/가구/세대 등 열명별 대상'
    if c=='유사_업종_점포_수':note='전체 점포 수의 작업 정의: 점포_수+프랜차이즈_점포_수. 2021·2025 대상행 항등식 확인'
    if c=='점포_수':note='일반 점포 수. 전체 점포 수와 구분; 당기 폐업 포함 정의 확인'
    if c.startswith('당월_'):note='원열명은 당월이나 행의 시간 키는 분기. 월별 관측으로 분해하지 않음'
    if c=='은행_수':note='주분석 대상 31,440행 중 21,820행 결측, 모든 상권 전기간 불변. 투입 보류'
    if any('시간대_건수~' in x for x in v['names']):note='원자료 한글 시간대 건수 열명의 오기 후보. 영문 시간대 대응은 추정 매핑이며 제공기관 확인 전 사용 보류'
    if scope=='배후지' and fam=='집객시설':note+='; 보유 2021~2025 범위 중 2025년 4개 분기만 존재'
    linked=[r['변수코드'] for r in rows if c in r['원자료_열'] and (fam in r['원자료_열'] or r['구분']=='식별자') and (('배후지' in r['분석범위'])==(scope=='배후지') or r['분석범위']=='공통')]
    usage='채택 변수의 재료' if linked else '통제 후보·미채택(정의·필요성 검토 후 선택)'
    if '코드' in c or c.endswith('_명') or '좌표' in c:usage='식별·분류·결합 정보'
    raw_rows.append(dict(자료군=fam,공간=scope,표준열명=c,실제열명=' / '.join(sorted(v['names'])),단위=unit,관측분기수=len(v['periods']),출처파일=';'.join(sorted(set(v['files']))),주의=note,분석지위=usage,연결변수=';'.join(linked)))

principles=[
 ('분석 범위','주분석','2021Q1~2025Q4. 골목상권(A)·발달상권(D)·전통시장(R). 관광특구(U) 제외. 1644개는 보유 점포/영역의 범위이며 성과별 완전 관측 표본 수와 다름.'),
 ('분석 범위','배후지 보조','주분석 상권의 성과를 종속변수로 유지하고 골목상권 배후지 특성을 추가. 배후지를 별도의 동일한 community 표본으로 합치지 않음.'),
 ('분류','내구성','충격 속 기능 유지. 제한 업종 의존·과거 수요 안정성·기존 영업기간은 관련 조건의 대리이며 내구성 자체의 검증 척도 아님. 당기 폐업·매출 하락은 결과.'),
 ('분류','가외성','대체·분산 가능성. 다양성·유효 업종 수·동일 기능 복수 공급의 후보. 실제 유휴역량·협력·서비스 대체를 입증하지 않음.'),
 ('분류','자원부존성','자원의 식별·동원·활용. 잠재 수요 기반, 내부 조직 활동, 추가 금융 접근·여력. 단순 자원량과 실제 동원 능력 구분.'),
 ('분류','신속성','대응 착수 소요일은 설명 후보. 사건 후 반등 기간은 결과. 현재 분기 자료만으로 초기 코로나19 대응속도 측정 불가.'),
 ('분류','적응성','학습·평가·대응 변경은 과정·역량 후보. 업종 구성 변화는 결과적 재편. 동일 재편 지표를 동시 원인과 결과로 쓰지 않음.'),
 ('분류','연대성','상권을 넘는 기관·다른 상권과의 자원 접근·공동 논의·협력. 기관 존재, 일반 대출 이용, 지리적 인접성과 구분.'),
 ('분류','경쟁 조건','경쟁성은 채택한 6Rs에 임의로 추가하지 않음. 배후지 동일업종 공급·구성 유사도는 보조 설명·통제로 두어 가외성과 비교.'),
 ('분류','민감성과 대응력','6Rs와 별개의 분석 관점. 한 변수의 주 구성요소를 하나 지정하고 중복 합산하지 않음. 설명변수는 합성 취약성 점수로 만들지 않음.'),
 ('정의','점포 분모','N=Σ유사_업종_점포_수. 2021·2025에서 점포_수+프랜차이즈_점포_수와 동일. 제공 폐업률과 일부 차이가 있어 재계산률로 명시하고 예외 조사.'),
 ('정의','다양성','Shannon=−Σp ln p; HHI=Σp²; HHI 역수=1/Σp²; 1−HHI는 다른 지수. 기본 Shannon, 다른 지수는 대안 모형.'),
 ('결측','진짜 0과 미상','명부에 없음·파일에 행 없음·빈칸은 0이 아님. 상인조직·지원 비참여는 기록 포괄성과 비참여 확인 후 0.'),
 ('시점','전년 비교·시차','전년 비교는 2022~2025. 분기 연속성 확인 후 t−4. t−1이 기계적으로 바로 이전 행이 되지 않도록 시간 키로 결합.'),
 ('시점','예측 정보 공개 시점','단순 t−1 관측과 예측 당시 공개 자료를 구분. 제공 지연을 반영한 이용 가능 시점 사용. 과거 원자료 빈티지 미확보 시 의사 실시간 검증이라고 명시.'),
 ('시간 비교','2024 공간 집계 변경','표준단위구역 변경과 보유 과거 파일의 소급 일치 여부 미확인. 연도 연결을 확정된 동일 경계 패널로 간주하지 않음.'),
 ('공간 비교','배후지 형상','통계 1090개와 영역 1071개 차이. 원상권 포함 여부·배후지 간 중첩·코드 이력 확인 전 차감 또는 합산 금지. 분모 면적 일치 필요.'),
 ('자료 범위','배후지 시설','관측기간 내 집객시설 배후지 파일은 2025Q1~Q4만. 이전 16분기에 소급·보간하지 않음.'),
 ('통제','선택적 투입','구성요소 외 변수는 통제 후보. 모두 일괄 투입하지 않음. 상권 고정효과에 흡수되는 유형·면적·지역 주효과, 구성비 합계, 다중공선성 점검.'),
 ('자료 범위','조직·금융·연대성','상권분석서비스만으로 직접 측정할 수 없음. 추가 행정·기관 기록을 확보한 심화 표본에서만 분석.'),
 ('해석','양적·질적 결과','매출·점포·개폐업과 업종·기능 구성 변화를 분리. 재편의 크기만으로 성공적 적응 판정 금지.'),
 ('근거','이론과 연구자 분류','6Rs는 하현상 외(2014)의 구분. 각 상권 지표와의 연결 및 산식 채택은 본 연구의 조작화이며 원문의 검증된 척도가 아님.'),
 ('파일','기존 코드북','processed/0_codebook.xlsx는 원본 보존. 초안의 변수를 검토해 길단위·유동인구 중복 통합, 소비여력 명칭 보류, HHI 역수 구분, 유형 통제로 정리.')]

sources=[
 ('T1','하현상 외(2014)','지역사회 재난 리질리언스 연구의 비판적 고찰과 행정학적 제언, pp.429–436, 444–445','https://journal.kci.go.kr/jrsd_ipaid/archive/articlePdf?artiId=ART001952915'),
 ('T2','Martin & Sunley(2015; 온라인 2014)','On the notion of regional economic resilience: conceptualization and explanation','https://doi.org/10.1093/jeg/lbu015'),
 ('T3','이슬·김태건·김갑성(2022)','코로나19 발생에 따른 서울시 골목상권 유형별 회복탄력성 및 영업 위기에 관한 분석','https://doi.org/10.19172/KREAA.28.2.1'),
 ('T4','유현지(2021)','코로나19와 서울시 골목상권, 한국지역개발학회지 33(3), 45–76','02_references/literature/코로나19와_서울시_골목상권.pdf'),
 ('T5','이윤명·김태형(2018)','서울시 전통시장 경제 활성화를 위한 시장 운영 및 입지 특성 분석, 19(2), 105–118','02_references/literature/traditional market/서울시 전통시장 경제 활성화를 위한 시장 운영 및 입지 특성 분석.pdf.pdf'),
 ('T6','이현정·안영수·여관현(2021)','서울시 전통시장 변화의 영향요인 탐색과 정책적 함의, 22(4), 23–42','02_references/literature/traditional market/서울시 전통시장 변화의 영향요인 탐색과 정책적 함의.pdf.pdf'),
 ('T7','김원배·신혜원(2013)','한국의 경제위기와 지역 탄력성, 국토연구 79, 3–21; 본 연구는 점포 구성 결과로 응용','02_references/literature/한국의 경제위기와 지역 탄력성.pdf'),
 ('D1','서울시 제공정보','seoul_info.pdf: 분기·점포·매출·인구 정의. 오래된 제공 기간 안내는 현재 공지와 구분','03_data/raw/서울시 상권분석서비스/seoul_info.pdf'),
 ('D2','서울시 점포-상권','전체 점포 정의와 2024년 공간 변경 공지; 2026-10-10~11 확인','https://data.seoul.go.kr/dataList/OA-15577/S/1/datasetView.do'),
 ('D3','서울시 추정매출-상권','공간 기준 변경 및 2021년 이후 제공 공지','https://data.seoul.go.kr/dataList/OA-15572/F/1/datasetView.do'),
 ('D4','서울시 점포-상권배후지','배후지 점포 자료 제공 페이지','https://data.seoul.go.kr/dataList/OA-15578/S/1/datasetView.do'),
 ('D5','서울시 영역-상권배후지','좌표계 EPSG:5181 안내, 과거 경계 이력 별도 확인','https://data.seoul.go.kr/dataList/OA-22159/S/1/datasetView.do')]

def writecsv(name,data):
    with (OUT/name).open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(data[0]));w.writeheader();w.writerows(data)

assert len({r['변수코드'] for r in rows})==len(rows)
for r in rows:
    u=r['단위']; code=r['변수코드']
    if r['구분']=='식별자': dtype,domain='character','원문 식별 코드. quarter는 20211~20254'
    elif u=='범주':dtype,domain='factor','원자료 범주; district_type은 A/D/R'
    elif u=='0/1':dtype,domain='integer','0, 1, NA'
    elif u=='0~1':dtype,domain='double','0≤x≤1'
    elif u=='%p':dtype,domain='double','−100≤x≤100'
    elif u in ['개','건','업종 수','일','분기'] and code!='function_count_change_g':dtype,domain='integer 또는 제공 추계값','x≥0; 반등 분기 미관측은 검열'
    else:dtype,domain='double','산식·원자료 정의에 따름; 로그비·변화량은 음수 가능'
    r['자료형_R']=dtype;r['허용범위']=domain
writecsv('변수코드북.csv',rows)
writecsv('원자료열사전.csv',raw_rows)
writecsv('파일검증.csv',file_rows)
industry_rows=[dict(업종코드=k,업종명=v,대분류={'CS1':'외식업','CS2':'서비스업','CS3':'소매업'}.get(k[:3],'확인 필요'),기능군='미확정',대면제한분류='정책별 대응표 필요') for k,v in sorted(industries.items())]
writecsv('업종코드.csv',industry_rows)
payload={'variables':rows,'raw':raw_rows,'files':file_rows,'industries':industry_rows,'principles':[dict(구분=a,항목=b,적용원칙=c) for a,b,c in principles],'sources':[dict(ID=a,저자_기관=b,근거=c,위치=d) for a,b,c,d in sources]}
(OUT/'codebook.json').write_text(json.dumps(payload,ensure_ascii=False,indent=2),encoding='utf-8')
for name in ['source_inventory.json','validation_summary.json']:
    shutil.copy2(WORK/name,OUT/name)

md=['# 서울시 상권분석서비스 변수 코드북 (2021~2025)',
    '작성일: 2026-10-11. 자료에 기초한 측정 설계안이며 실증분석 결과가 아니다.',
    '## 분석 범위와 핵심 판단',
    '주분석은 골목상권(A)·발달상권(D)·전통시장(R)이다. 보유 점포·영역 기준 1,644개(골목 1,090, 발달 249, 전통 305)이며, 관광특구 6개는 제외한다. 지표별 관측 가능 표본은 달라진다.',
    '상권배후지는 골목상권 1,090개에 관한 보조자료이다. 배후지 특성은 해당 상권의 성과와 연결하되, 배후지를 주분석 표본으로 합치거나 전통시장·발달상권에 동일 자료가 존재한다고 가정하지 않는다.',
    '경쟁 조건은 6Rs의 별도 구성요소가 아니다. 주변 동일업종 공급이 대체 가능성을 제공하는지, 경쟁 압력과 연결되는지를 구분하여 보조 설명·통제변수로 비교한다.',
    '## 구성요소와 변수 역할',
    '| 구성요소 | 설명·과정 변수의 후보 | 결과와 구분할 점 |',
    '|---|---|---|',
    '| 내구성 | 제한 업종 의존, 과거 수요 안정성, 기존 영업 지속성 | 당기 폐업·매출 하락은 결과. 직접 내구성 척도와 대리 조건 구분 |',
    '| 가외성 | Shannon, HHI 역수, 동일 기능 복수 공급 | 다양성은 잠재 분산·대체 조건. 실제 유휴역량 아님 |',
    '| 자원부존성 | 수요 기반, 조직의 동원·실행, 금융적 대응 자원 | 자원량과 실제 동원 능력 구분 |',
    '| 신속성 | 공동 대응 착수 소요일(추가 자료) | 사건 후 반등 분기 수는 보조 결과 |',
    '| 적응성 | 공동 학습·평가·대응 변경(추가 자료) | 업종·기능 구성 변화는 질적 결과 |',
    '| 연대성 | 외부 자원 접근과 실제 공동 논의·협력(추가 자료) | 상권 밖 기관 존재나 일반 대출 이용만으로 측정하지 않음 |',
    '| 구성요소 외 | 유형·면적·집적·구성·시기·지역·입지·과거 실적 | 통제 후보. 모두 자동 투입하지 않음 |',
    '위 분류는 하현상 외의 개념을 상권에 적용한 연구자 조작화이다. 특히 내구성·자원부존성 대리변수는 원문의 검증 척도가 아니다. 민감성·대응력과 6Rs는 서로 다른 분류 관점이며 중복 합산하지 않는다.',
    '## 계산·결합 원칙']
for a,b,c in principles: md.append(f'- **{b}**: {c}')
md += ['## 자료 검증에서 확인한 사항',
 '원자료 CSV 35개를 읽어 열 이름과 분석기간·유형 필터 후 키·코드를 확인했다. 영역 자료는 시간 키가 없어 별도 취급하였다. 이 검증은 값 전체의 정합성이나 2024년 공간 비교성 해결을 뜻하지 않는다.',
 '2025년 점포(상권·배후지), 2025년 추정매출(배후지)은 소문자 영문 헤더다. 원자료열사전에 명시적 대응을 보존하였다. 시간대 매출 건수의 한글 헤더에 `시간대_건수~06` 등 오기 후보가 있어 해당 파생변수는 채택하지 않았다.',
 '배후지 집객시설은 2021~2025 필터에서 2025년 4개 분기만 확인됐다. 배후지 영역은 1,071개여서 1,090개 통계와 차이가 있다. 면적 기반 지표와 공간 중첩 처리는 코드·시점·형상 검증 후 산출한다.',
 '주분석 은행 수는 31,440행 중 21,820행이 결측이고 상권별 전 기간 값이 불변이다. 현재는 설명·예측 입력에서 보류한다.',
 '점포 분모는 2021년 301,814행 및 2025년 302,701행 모두에서 `유사_업종_점포_수 = 점포_수 + 프랜차이즈_점포_수`였다. 양의 분모 행 중 제공 폐업률과 재계산률 차이가 0.51%p를 넘는 행은 각각 161행, 142행이었다. 제공률과 동일하다고 주장하지 않고 재계산 폐업률의 정의를 명시한다.',
 '## 변수별 상세 코드북',
 f'총 {len(rows)}개 변수·변수군을 수록했다. `_k`, `_g`는 업종별·기능군별로 확장되는 변수군이며 열 하나를 뜻하지 않는다. 기능군 100개 업종 대응은 아직 확정하지 않았다.']
for r in rows:
    md += [f"### {r['변수코드']} — {r['변수']}", f"- 분류: {r['구분']} / {r['구성요소']} / {r['특성군']}",f"- 범위·상태: {r['분석범위']} / {r['채택상태']}",f"- 산식: `{r['조작적정의_산식']}`",f"- 원자료: {r['원자료_열']}",f"- 단위·시간: {r['단위']} / {r['시간배치']}",f"- 자료형·허용범위: {r['자료형_R']} / {r['허용범위']}",f"- 해석: {r['개념연결_예상관계']}",f"- 결측: {r['결측_처리']}",f"- 주의: {r['해석상주의']}",f"- 근거: {r['근거']}"]
md+=['## 출처와 확인 경로']
for a,b,c,d in sources:
    link=d if d.startswith('http') else (Path(r'C:\Users\chwou\Desktop\graduate')/d).as_posix()
    md.append(f'- {a}. [{b}](<{link}>). {c}.')
md+=['## 파일 구성',
    '- `변수코드북.xlsx`: 개념·산식·원자료·시차·결측·근거와 원자료 열/파일 검증을 함께 필터링하는 코드북.',
    '- `변수코드북.csv`: R에서 `readr::read_csv()`로 읽을 수 있는 UTF-8 BOM 표.',
    '- `원자료열사전.csv`: 실제 헤더와 표준화 대응. `파일검증.csv`: 출처별 행·분기·유형·중복 키 확인.',
    '- `업종코드.csv`: 실제 관측 100개 생활밀접업종. 대분류는 코드 접두부, 기능군·정책 제한 분류는 미확정.',
    '- 기존 `03_data/processed/0_codebook.xlsx`는 수정하지 않았다.']
(OUT/'코드북_설명.md').write_text('\n\n'.join(md)+'\n',encoding='utf-8')
print('OUTPUT',OUT)
print('VARIABLES',len(rows),'RAW FIELDS',len(raw_rows),'FILES',len(file_rows),'INDUSTRIES',len(industry_rows))
