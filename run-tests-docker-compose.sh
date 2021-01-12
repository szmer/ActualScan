#
# *Intra-service tests*.
#
# These are the tests running inside the containers. They have the network interactrions mocked by
# test suites so they are independent on all the other containers functioning propeely.
#
# TODO use junit-xml or a similar format to collect all these tests to one source of truth.
#
echo '==========================='
echo '====== Website tests ======'
echo '==========================='
docker-compose exec website pytest
echo '================================='
echo '======== Omnivore2 tests ========'
echo '================================='
docker-compose exec omnivore2 pytest
echo '================================='
echo '====== Speechtractor tests ======'
echo '================================='
# We change the port to something else before running the test speechtractor server.
docker-compose exec speechtractor bash -c "export SPEECHTRACTOR_PORT=3758 && sbcl --eval '(ql:quickload :speechtractor)' \
   --eval '(setf speechtractor::*server-enter-debug-p* nil speechtractor::*server-silentp* t speechtractor::*log-requests-p* nil)' \
   --eval '(setf *package* (find-package :speechtractor-test))' --eval '(run-package-tests)' --eval '(sb-ext:quit)'"

#
# Integration tests (TODO)
#
