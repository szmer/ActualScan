from omnivore2_conf import (
        COMMON_NGRAMS_COUNT_FOR_MERGING, LANGUAGES_SUPPORTED,
        MINIMAL_GOOD_PERIOD_LENGTH, MAXIMAL_GOOD_PERIOD_LENGTH
        )

def text_char_ngrams(n, text):
    "Return all string character n-grams as a sorted list of strings."
    ngrams = []
    for char_n, char in enumerate(text):
        if (char_n+n) < len(text):
            ngrams.append(text[char_n:char_n+n])
    return sorted(ngrams)

def common_strings_count(list1, list2):
    return len(set(list1).intersection(set(list2)))

def periods_from_spacy_sentences(spacy_sentences):
    """
    Transform a sequence of spacy sentences into a list of periods (combining possibly few sentences)
    as strings. The additional return values contain word and sentence counts of the subsequent
    periods.
    """
    spacy_sentences = list(spacy_sentences)
    periods = []
    period_sent_counts = []
    period_word_counts = []
    for sent_n, sentence in enumerate(spacy_sentences):
        if not periods:
            periods.append(str(sentence))
            period_sent_counts.append(1)
            period_word_counts.append(len(sentence))
        elif (period_word_counts[-1] < MINIMAL_GOOD_PERIOD_LENGTH # continue a too short period
                # join a short last sentence:
                or (((sent_n+1) == len(spacy_sentences))
                    and len(sentence) < MINIMAL_GOOD_PERIOD_LENGTH)):
            periods[-1] += ' ' + str(sentence)
            period_sent_counts[-1] += 1
            period_word_counts[-1] += len(sentence)
        elif (not (sent_n+2 == len(spacy_sentences)
                    and len(spacy_sentences[-1]) < MINIMAL_GOOD_PERIOD_LENGTH)
                and (period_word_counts[-1]+len(sentence)) < MAXIMAL_GOOD_PERIOD_LENGTH
                and common_strings_count(text_char_ngrams(3, str(sentence)),
                    text_char_ngrams(3, periods[-1]))
                >= COMMON_NGRAMS_COUNT_FOR_MERGING):
            periods[-1] += ' ' + str(sentence)
            period_sent_counts[-1] += 1
            period_word_counts[-1] += len(sentence)
        else:
            periods.append(str(sentence))
            period_sent_counts.append(1)
            period_word_counts.append(len(sentence))
    return periods, period_sent_counts, period_word_counts 

def lang_text_field_code(lang_code):
    if lang_code in LANGUAGES_SUPPORTED:
        return 'text_'+lang_code
    else:
        return 'text_xx'

def period_text(period_dict):
    "The text of the textual period, retrieved from the appropriate field for its language."
    return period_dict[lang_text_field_code(period_dict['language_code'])]
