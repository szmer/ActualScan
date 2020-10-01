#
# *Intra-service tests*.
#
# These are the tests running inside the containers. They have the network interactrions mocked by
# test suites so they are independent on all the other containers functioning propeely.
#
# TODO use junit-xml or a similar format to collect all these tests to one source of truth.
#
echo '====== Website tests ======'
docker-compose exec website pytest
echo '====== Scrapy tests ======'
docker-compose exec scrapy pytest
echo '====== Speechtractor tests ======'
# We change the port to something else before running the test speechtractor server.
docker-compose exec speechtractor bash -c "export SPEECHTRACTOR_PORT=3758 && sbcl --eval '(ql:quickload :speechtractor)' \
   --eval '(setf speechtractor::*server-enter-debug-p* nil speechtractor::*server-silentp* t speechtractor::*log-requests-p* nil)' \
   --eval '(setf *package* (find-package :speechtractor-test))' --eval '(run-package-tests)' --eval '(sb-ext:quit)'"

#
# Integration tests (TODO)
#
