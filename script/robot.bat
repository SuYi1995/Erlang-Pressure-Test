cd ../config
werl +P 1024000 -smp +S 1 -name robot@192.168.1.97 -setcookie game -boot start_sasl -config elog -pa ../ebin -s main start -extra tester 192.168.1.97 8001 robot
pause
