
function OtrixDateFormat(){this.SHORT_MONTH_NAMES=["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];this.LONG_MONTH_NAMES=["January","February","March","April","May","June","July","August","September","October","November","December"];this.SHORT_DAY_NAMES=["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];this.LONG_DAY_NAMES=["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];}
OtrixDateFormat.prototype.format=function(date,format,sMl,lMl,sDl,lDl){format=format+"";var shortMonthList=(otrix.isString(sMl)?sMl.split(","):this.SHORT_MONTH_NAMES);var longMonthList=(otrix.isString(lMl)?lMl.split(","):this.LONG_MONTH_NAMES);var shortDayList=(otrix.isString(sDl)?sDl.split(","):this.SHORT_DAY_NAMES);var longDayList=(otrix.isString(lDl)?lDl.split(","):this.LONG_DAY_NAMES);var result="";var i_format=0;var c="";var token="";var y=date.getYear()+"";var M=date.getMonth()+1;var d=date.getDate();var E=date.getDay();var H=date.getHours();var m=date.getMinutes();var s=date.getSeconds();var value=new Object();if(y.length<4){y=""+(y-0+1900);}
value["y"]=""+y;value["yyyy"]=y;value["yy"]=y.substring(2,4);value["M"]=M;value["MM"]=otrix.pad(M,2,"0");value["MMM"]=shortMonthList[M-1];value["MMMM"]=longMonthList[M-1];value["d"]=d;value["dd"]=otrix.pad(d,2,"0");value["E"]=shortDayList[E];value["EE"]=longDayList[E];value["H"]=H;value["HH"]=otrix.pad(H,2,"0");if(H===0){value["h"]=12;}
else if(H>12){value["h"]=H-12;}
else{value["h"]=H;}
value["hh"]=otrix.pad(value["h"],2,"0");if(H>11){value["K"]=H-12;}else{value["K"]=H;}
value["k"]=H+1;value["KK"]=otrix.pad(value["K"],2,"0");value["kk"]=otrix.pad(value["k"],2,"0");if(H>11){value["a"]="PM";}
else{value["a"]="AM";}
value["m"]=m;value["mm"]=otrix.pad(m,2,"0");value["s"]=s;value["ss"]=otrix.pad(s,2,"0");while(i_format<format.length){c=format.charAt(i_format);token="";while((format.charAt(i_format)==c)&&(i_format<format.length)){token+=format.charAt(i_format++);}
if(!otrix.isNull(value[token])){result=result+value[token];}
else{result=result+token;}}
return result;};function _isInteger(val){var digits="1234567890";for(var i=0;i<val.length;i++){if(digits.indexOf(val.charAt(i))==-1){return false;}}
return true;}
function _getInt(str,i,minlength,maxlength){for(var x=maxlength;x>=minlength;x--){var token=str.substring(i,i+x);if(token.length<minlength){return null;}
if(_isInteger(token)){return token;}}
return null;}
OtrixDateFormat.prototype.parse=function(val,format,sMl,lMl,sDl,lDl){val=val+"";format=format+"";var i_val=0;var i_format=0;var c="";var token="";var token2="";var x,y;var now=new Date();var year=now.getYear();var month=now.getMonth()+1;var date=1;var hh=now.getHours();var mm=now.getMinutes();var ss=now.getSeconds();var ampm="";var shortMonthList=(otrix.isString(sMl)?sMl.split(","):this.SHORT_MONTH_NAMES);var longMonthList=(otrix.isString(lMl)?lMl.split(","):this.LONG_MONTH_NAMES);var shortDayList=(otrix.isString(sDl)?sDl.split(","):this.SHORT_DAY_NAMES);var longDayList=(otrix.isString(lDl)?lDl.split(","):this.LONG_DAY_NAMES);while(i_format<format.length){c=format.charAt(i_format);token="";while((format.charAt(i_format)==c)&&(i_format<format.length)){token+=format.charAt(i_format++);}
if(token=="yyyy"||token=="yy"||token=="y"){if(token=="yyyy"){year=_getInt(val,i_val,4,4);}
if(token=="yy"){year=_getInt(val,i_val,2,2);}
if(token=="y"){year=_getInt(val,i_val,2,4);}
if(otrix.isNull(year)){return null;}
i_val+=year.length;if(year.length==2){if(year>70){year=1900+(year-0);}
else{year=2000+(year-0);}}}
else if(token=="MMMM"){month=0;for(var i=0;i<longMonthList.length;i++){var month_name=longMonthList[i];if(val.substring(i_val,i_val+month_name.length).toLowerCase()==month_name.toLowerCase()){month=i+1;i_val+=month_name.length;break;}}
if((month<1)||(month>12)){return null;}}
else if(token=="MMM"){month=0;for(var i=0;i<shortMonthList.length;i++){var month_name=shortMonthList[i];if(val.substring(i_val,i_val+month_name.length).toLowerCase()==month_name.toLowerCase()){month=i+1;i_val+=month_name.length;break;}}
if((month<1)||(month>12)){return null;}}
else if(token=="EE"){for(var i=0;i<longDayList.length;i++){var day_name=longDayList[i];if(val.substring(i_val,i_val+day_name.length).toLowerCase()==day_name.toLowerCase()){i_val+=day_name.length;break;}}}
else if(token=="E"){for(var i=0;i<shortDayList.length;i++){var day_name=shortDayList[i];if(val.substring(i_val,i_val+day_name.length).toLowerCase()==day_name.toLowerCase()){i_val+=day_name.length;break;}}}
else if(token=="MM"||token=="M"){month=_getInt(val,i_val,token.length,2);if(month==null||(month<1)||(month>12)){return null;}
i_val+=month.length;}
else if(token=="dd"||token=="d"){date=_getInt(val,i_val,token.length,2);if(date==null||(date<1)||(date>31)){return null;}
i_val+=date.length;}
else if(token=="hh"||token=="h"){hh=_getInt(val,i_val,token.length,2);if(hh==null||(hh<1)||(hh>12)){return null;}
i_val+=hh.length;}
else if(token=="HH"||token=="H"){hh=_getInt(val,i_val,token.length,2);if(hh==null||(hh<0)||(hh>23)){return null;}
i_val+=hh.length;}
else if(token=="KK"||token=="K"){hh=_getInt(val,i_val,token.length,2);if(hh==null||(hh<0)||(hh>11)){return null;}
i_val+=hh.length;}
else if(token=="kk"||token=="k"){hh=_getInt(val,i_val,token.length,2);if(hh==null||(hh<1)||(hh>24)){return 0;}
i_val+=hh.length;hh--;}
else if(token=="mm"||token=="m"){mm=_getInt(val,i_val,token.length,2);if(mm==null||(mm<0)||(mm>59)){return 0;}
i_val+=mm.length;}
else if(token=="ss"||token=="s"){ss=_getInt(val,i_val,token.length,2);if(ss==null||(ss<0)||(ss>59)){return 0;}
i_val+=ss.length;}
else if(token=="a"){if(val.substring(i_val,i_val+2).toLowerCase()=="am"){ampm="AM";}
else if(val.substring(i_val,i_val+2).toLowerCase()=="pm"){ampm="PM";}
else{return null;}
i_val+=2;}
else{if(val.substring(i_val,i_val+token.length)!=token){return null;}
else{i_val+=token.length;}}}
if(i_val!=val.length){return null;}
if(month==2){if(((year%4==0)&&(year%100!=0))||(year%400==0)){if(date>29){return null;}}
else{if(date>28){return null;}}}
if((month==4)||(month==6)||(month==9)||(month==11)){if(date>30){return null;}}
if(hh<12&&ampm=="PM"){hh=hh-0+12;}
else if(hh>11&&ampm=="AM"){hh-=12;}
var newdate=new Date(year,month-1,date,hh,mm,ss);return newdate;};otrix.dateFormat=new OtrixDateFormat();