import os

SOLR_HOST = os.environ.get('SOLR_HOST', 'solr')
SOLR_PORT = os.environ.get('SOLR_PORT', 8983)
SOLR_CORE = os.environ.get('SOLR_CORE', 'ascan')

MINIMAL_GOOD_PERIOD_LENGTH = 30 # in words
MAXIMAL_GOOD_PERIOD_LENGTH = 50
COMMON_NGRAMS_COUNT_FOR_MERGING = 15

# The languages that get their own field and treatment in Solr (the rest goes to text_xx).
LANGUAGES_SUPPORTED = set(['en'])

MINIMUM_CONTEXT_SIZE = 200
MAXIMUM_CONTEXT_SIZE = 800
# the time, in hours, after which a text period can get a new contextual analysis
RECLASSIFICATION_TIME = 72
