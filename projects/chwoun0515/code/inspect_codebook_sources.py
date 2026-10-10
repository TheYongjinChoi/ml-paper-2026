from pathlib import Path
import json
import pandas as pd
from pypdf import PdfReader
ROOT=Path(__file__).resolve().parents[1]
RAW=Path(r'C:\Users\chwou\Desktop\graduate\03_data\raw\서울시 상권분석서비스')
OUT=ROOT/'codebook'
OUT.mkdir(exist_ok=True)
inventory=[]
industries={}
for p in sorted(RAW.rglob('*.csv')):
    for enc in ['utf-8-sig','cp949']:
        try:
            header=pd.read_csv(p,encoding=enc,nrows=2,dtype=str)
            break
        except UnicodeDecodeError: pass
    cols=list(header.columns)
    selected=[c for c in cols if any(k in c.lower() for k in ['분기','상권','업종_코드','업종_코드_명','stdr_yyqu','trdar','svc_induty'])]
    frame=pd.read_csv(p,encoding=enc,usecols=selected,dtype=str)
    q=next((c for c in cols if '분기' in c or c.lower()=='stdr_yyqu_cd'),None)
    idc=next((c for c in cols if c.lower() in ['상권_코드','상권배후지_코드','상권배후지_구분_코드','trdar_cd']),None)
    tc=next((c for c in cols if c.lower() in ['상권_구분_코드','trdar_se_cd']),None)
    tn=next((c for c in cols if c.lower() in ['상권_구분_코드_명','trdar_se_cd_nm']),None)
    ic=next((c for c in cols if c.lower() in ['서비스_업종_코드','svc_induty_cd']),None)
    inn=next((c for c in cols if c.lower() in ['서비스_업종_코드_명','svc_induty_cd_nm']),None)
    target=frame[frame[q].between('20211','20254')] if q else frame
    main=target[target[tc].isin(['A','D','R'])] if tc else target
    keys=[c for c in [q,idc,ic] if c]
    item=dict(file=p.name,path=str(p),family='영역' if '영역' in p.name else p.parent.name,scope='배후지' if '배후지' in p.name else '상권',encoding=enc,columns=cols,rows=len(frame),target_rows=len(target),main_rows=len(main),periods=sorted(target[q].dropna().unique().tolist()) if q else [],districts=int(main[idc].nunique()) if idc else None,types=target[[tc,tn]].drop_duplicates().values.tolist() if tc and tn else [],duplicate_keys=int(main.duplicated(keys).sum()) if keys else None, sample_ids=main[idc].dropna().unique()[:4].tolist() if idc else [],type_counts=main.groupby(tc)[idc].nunique().to_dict() if tc and idc else {})
    inventory.append(item)
    if ic and inn:
        for code,name in main[[ic,inn]].drop_duplicates().values:
            industries[code]=name
    print(p.name,len(cols),len(main),item['districts'],flush=True)
(OUT/'source_inventory.json').write_text(json.dumps(inventory,ensure_ascii=False,indent=2),encoding='utf-8')
(OUT/'industry_codes.json').write_text(json.dumps(industries,ensure_ascii=False,indent=2),encoding='utf-8')
texts=[]
for p in RAW.glob('*.pdf'):
    reader=PdfReader(p)
    for i,page in enumerate(reader.pages):
        texts.append(f'\nFILE {p.name} PAGE {i+1}\n'+(page.extract_text() or ''))
(OUT/'source_metadata_extract.txt').write_text('\n'.join(texts),encoding='utf-8')
print('COMPLETE',len(inventory),len(industries))
