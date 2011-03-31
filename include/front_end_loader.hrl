% LogFormat "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-agent}i\"" combined
% 10.61.104.254 - - [23/Mar/2011:00:00:01 +0000] "GET /app_metrics HTTP/1.1" 200 458 "-" "curl/7.15.5 (x86_64-redhat-linux-gnu) libcurl/7.15.5 OpenSSL/0.9.8b zlib/1.2.3 libidn/0.6.5"

-record (request, {method, path, http_version}).
-record (log_line, {host, remote_logname, remote_user, time, request, status, response_bytesize, referrer, user_agent}).