import fs from 'node:fs/promises';
import {Workbook, SpreadsheetFile} from '@oai/artifact-tool';
const out='C:/Users/chwou/Desktop/graduate/03_data/processed/서울시상권_코드북_2021_2025';
const data=JSON.parse(await fs.readFile(`${out}/codebook.json`,'utf8'));
const workbook=Workbook.create();
const support='C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/codebook';
const alphabet=n=>{let s='';for(n++;n>0;n=Math.floor((n-1)/26))s=String.fromCharCode(65+(n-1)%26)+s;return s};
const specs=[
 ['변수코드북',data.variables,[15,23,24,29,28,23,65,65,24,33,33,62,62,72,40,28,52]],
 ['분류원칙',data.principles.concat(data.sources.map(x=>({구분:'출처',항목:`${x.ID} ${x.저자_기관}`,적용원칙:`${x.근거}\n${x.위치}`}))),[18,30,110]],
 ['원자료열사전',data.raw,[20,14,43,65,40,15,45,90,48,65]],
 ['파일검증',data.files,[14,20,14,75,14,17,17,17,17,17,19,19,17,42,115]],
 ['업종코드',data.industries,[20,34,20,20,35]]
];
for(let idx=0;idx<specs.length;idx++){
 const [name,rows,widths]=specs[idx];
 const sheet=workbook.worksheets.add(name);
 const headers=Object.keys(rows[0]);
 const matrix=[headers,...rows.map(r=>headers.map(k=>r[k]??null))];
 const end=alphabet(headers.length-1);
 const all=sheet.getRange(`A1:${end}${matrix.length}`);
 all.values=matrix;
 all.format.font={name:'Malgun Gothic',size:11,color:'#172C40'};
 all.format.wrapText=true;
 all.format.verticalAlignment='top';
 all.format.rowHeight=30;
 headers.forEach((h,i)=>sheet.getRange(`${alphabet(i)}1:${alphabet(i)}${matrix.length}`).format.columnWidth=widths[i]);
 const head=sheet.getRange(`A1:${end}1`);
 head.format.fill='#203B53';head.format.font={name:'Malgun Gothic',size:11,bold:true,color:'#FFFFFF'};
 head.format.rowHeight=34;head.format.horizontalAlignment='center';
 sheet.freezePanes.freezeRows(1);
 sheet.freezePanes.freezeColumns(name==='변수코드북'?5:2);
 sheet.showGridLines=false;
 const t=sheet.tables.add(`A1:${end}${matrix.length}`,true,`CodebookTable${idx+1}`);
 t.showFilterButton=true;
 for(let row=2;row<=matrix.length;row++){
  if(row%2===0)sheet.getRange(`A${row}:${end}${row}`).format.fill='#F2F5F8';
  const lines=Math.max(...matrix[row-1].map((value,i)=>String(value??'').split('\n').reduce((n,part)=>n+Math.max(1,Math.ceil([...part].reduce((a,c)=>a+(c.charCodeAt(0)>255?2:1),0)/(widths[i]*0.85))),0)));
  sheet.getRange(`A${row}:${end}${row}`).format.rowHeight=Math.max(30,lines*17+10);
 }
 if(name==='업종코드'){sheet.getRange(`A2:${end}${matrix.length}`).format.rowHeight=30;}
 if(name==='파일검증'){sheet.getRange(`F2:J${matrix.length}`).setNumberFormat('#,##0');sheet.getRange(`M2:M${matrix.length}`).setNumberFormat('#,##0');}
 if(name==='변수코드북'){
  sheet.tabColor='#203B53';
  sheet.getRange(`K2:K${matrix.length}`).conditionalFormats.add('containsText',{text:'추가 자료',format:{fill:'#FFF1D6'}});
  sheet.getRange(`K2:K${matrix.length}`).conditionalFormats.add('containsText',{text:'보류',format:{fill:'#FBE8E6'}});
 }
}
workbook.recalculate();
console.log((await workbook.inspect({kind:'table',range:'변수코드북!A1:F5',include:'values',tableMaxRows:5,tableMaxCols:6,maxChars:1500})).ndjson);
console.log((await workbook.inspect({kind:'match',searchTerm:'#REF!|#DIV/0!|#VALUE!|#NAME\\?|#NUM!',options:{useRegex:true,maxResults:20},maxChars:1000})).ndjson);
for(const [name] of specs){
 const range=name==='분류원칙'?'A1:C6':name==='변수코드북'?'A1:F6':name==='원자료열사전'?'A1:D6':name==='파일검증'?'A1:E6':'A1:E9';
 try{
  const preview=await workbook.render({sheetName:name,range,scale:1,format:'png'});
  await fs.writeFile(`${support}/preview-${name}.png`,new Uint8Array(await preview.arrayBuffer()));
  console.log('Rendered',name);
 }catch(e){console.log('Render issue',name,e.message);}
}
const detail=await workbook.render({sheetName:'변수코드북',range:'G20:O23',scale:1,format:'png'});
await fs.writeFile(`${support}/preview-detail.png`,new Uint8Array(await detail.arrayBuffer()));
const types=await workbook.render({sheetName:'변수코드북',range:'P1:Q8',scale:1,format:'png'});
await fs.writeFile(`${support}/preview-types.png`,new Uint8Array(await types.arrayBuffer()));
const xlsx=await SpreadsheetFile.exportXlsx(workbook);
await xlsx.save(`${out}/변수코드북.xlsx`);
console.log('SAVED',`${out}/변수코드북.xlsx`);
