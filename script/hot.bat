
:: ���ŷ������������˫���Ľű������ȸ��޸Ĺ���erl�ļ���ע������ڵ�����cookie�����ö�Ӧ��������

:: ȱ���Ǹ���.hrl�ļ��Ļ��Ǳ��벻���ġ�

:: ����ͷ�ļ���Ҫ�ȸ��Ļ����������h:h() �ĳ� h:hh()


cd ../ebin
erl -name hot@192.168.1.97 -setcookie game -eval "case net_adm:ping('robot@192.168.1.97') of pang-> io:format(\"No nodes!!!\");_->h:h() end,	halt(1)"
pause