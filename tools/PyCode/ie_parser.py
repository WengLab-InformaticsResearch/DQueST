class Parser:

    def __init__(self,input):
        # type: (object) -> object
        """
        parse nlp results
        :param file: nlp results from CY
        """
        self.nlp_results = input
        self.bigger_words = ["\>", "\+", '≥', "more", 'above', "big", "great", "more", "excess", "high", "better", "exceed","old", "long", 'at', 'over']
        self.and_words = ["\-", "to", ":", "and"]
        self.smaller_words = ["\<", "\-", '≤', "less", "small", "low", "worse", "young", "short", 'most', 'below']
        self.current_words = ["now", "current", "ongoing", "present", "simultaneous", "undergoing", "recent", "concomitant"]
        self.longer_words = ["lease", "\>", "≥", "\+", "x", "more", "big", "great", "more", "excess", "high", "better", "exceed","old", "long", "beyond"]
        self.shorter_words = ["most", "\<", "≤", "\-", "less", "small", "low", "worse", "young", "short", "within", "with","last","past", "before","in"]
        self.between_words = ["-","~","--",'to', "and"]

        self.time_unit = ['hr',"hour","day","wks","week","mon","month","year","yr"]
        self.time2days = {'hr': 'hour', 'hour': 'hour', 'day': 'day', 'wks': 'week', 'week': 'week', 'mon': 'month','month': 'month', 'year': 'year', 'yr': 'year'}
        self.rate2days = {'hour': 0.04166667, 'day': 1, 'week': 7, 'month': 30.5, 'year': 365}
        self.digit = '(\d+[\.]*\d*)'

    def parse_line(self,output):
        """
        parsing line by line and output files
        :return: None
        """
        o = open(output, 'w+')
        with open(self.nlp_results) as f:
            for line in f:
                fields = line.rstrip().split('\t')
                temporal = fields[5]
                temporal_parsed = self.parse_temporal(temporal)
                value = fields[6]
                value_parsed = self.parse_value(value)
                row_append = line.rstrip() + '\t' + '\t'.join(temporal_parsed) + '\t' + '\t'.join(value_parsed) + '\n'
                # print(row_append)
                o.write(row_append)
        o.close()
        return None

    def parse_value(self, value):
        """
        parsing for value expression
        :param value: value field in nlp results
        :return: [min_value, max_value, value_unit]
        """
        value = value.lower()
        bigger_words = self.bigger_words
        smaller_words = self.smaller_words
        and_words = self.and_words
        digit = self.digit

        min_value = 'NA'
        max_value = 'NA'
        value_unit = 'NA'

        if (value == 'no_value'):
            return [min_value, max_value]
        else:
            for aw in and_words:
                min_value = 'NA'
                max_value = 'NA'
                value_unit = 'NA'
                p = re.compile('.*?' + digit + '.*?' + aw + '.*?' + digit + '(.*)')
                m = p.match(value)
                if (m):
                    min_value = m.group(1)
                    max_value = m.group(2)
                    min_value = float(min_value)
                    max_value = float(max_value)
                    if (min_value < max_value):
                        value_unit = m.group(3)
                        min_value = str(float(min_value))
                        max_value = str(float(max_value))
                        return [min_value, max_value, value_unit]

            for aw in bigger_words:
                min_value = 'NA'
                max_value = 'NA'
                value_unit = 'NA'
                p1 = re.compile('.*?' + digit + '(.*?)' + aw)
                p2 = re.compile(aw + '.*?' + digit + '(.*)')
                for p in [p1, p2]:
                    m = p.match(value)
                    if (m):
                        min_value = str(float(m.group(1)))
                        max_value = '+Inf'
                        value_unit = m.group(2)
                        return [min_value, max_value, value_unit]

            for aw in smaller_words:
                min_value = 'NA'
                max_value = 'NA'
                value_unit = 'NA'
                p1 = re.compile('.*?' + digit + '(.*?)' + aw)
                p2 = re.compile(aw + '.*?' + digit + '(.*)')
                for p in [p1, p2]:
                    m = p.match(value)
                    if m:
                        min_value = '-Inf'
                        max_value = str(float(m.group(1)))
                        value_unit = m.group(2)
                        return [min_value, max_value, value_unit]

            min_value = 'NA'
            max_value = 'NA'
            value_unit = 'NA'
            return [min_value, max_value, value_unit]

    def parse_temporal(self, temporal):
        """
        :param temporal: temporal field in nlp results
        :return: [min_value, max_value, value_unit]
        """
        temporal = temporal.lower()
        current_words = self.current_words
        bigger_words = self.longer_words
        smaller_words = self.shorter_words
        and_words = self.between_words
        unit = self.time_unit
        rate2days = self.rate2days
        digit = self.digit

        min_value = 'NA'
        max_value = 'NA'
        value_unit = 'NA'
        if temporal == 'no_temporal':
            return [min_value, max_value, value_unit]

            # scan current first.
        for aw in current_words:
            min_value = 'NA'
            max_value = 'NA'
            value_unit = 'NA'
            p = re.compile(aw)
            m = p.match(temporal)
            if m:
                return [min_value, max_value, value_unit]

        for aw in and_words:
            min_value = 'NA'
            max_value = 'NA'
            value_unit = 'NA'
            p = re.compile('.*?' + digit + '.*?' + aw + '.*?' + digit + '(.*)')
            m = p.match(temporal)
            if m:
                # change by Cong Liu
                # e.g. 3 to 5 years
                # min = -5
                # max = -3
                min_value = '-' + m.group(2)
                max_value = '-' + m.group(1)
                min_value = int(float(min_value))
                max_value = int(float(max_value))
                if (min_value < max_value):
                    value_unit = m.group(3)
                    value_unit = value_unit.strip()
                    if len(value_unit.split()) == 1:
                        p = re.compile('(' + '|'.join(unit) + ')')
                        m = p.match(value_unit)
                        if (m):
                            value_unit = self.time2days[m.group(1)]
                            rate = rate2days[value_unit]
                            min_value = str(int(float(min_value) * rate))
                            max_value = str(int(float(max_value) * rate))
                            return [min_value, max_value, value_unit]

        for aw in bigger_words:
            min_value = 'NA'
            max_value = 'NA'
            value_unit = 'NA'
            p1 = re.compile('.*?' + digit + '(.*?)' + aw)
            p2 = re.compile(aw + '.*?' + digit + '(.*)')
            for p in [p1, p2]:
                m = p.match(temporal)
                if m:
                # change by Cong Liu
                    # e.g. more than 5 years
                    # min = -Inf
                    # max = -5
                    min_value = '-Inf'
                    max_value = '-' + m.group(1)
                    value_unit = m.group(2)
                    value_unit = value_unit.strip()
                    if len(value_unit.split()) == 1:
                        p = re.compile('(' + '|'.join(unit) + ')')
                        m = p.match(value_unit)
                        if m:
                            value_unit = self.time2days[m.group(1)]
                            rate = rate2days[value_unit]
                            max_value = str(int(float(max_value) * rate))
                            return [min_value, max_value, value_unit]

        for aw in smaller_words:
            min_value = 'NA'
            max_value = 'NA'
            value_unit = 'NA'
            p1 = re.compile('.*?' + digit + '(.*?)' + aw)
            p2 = re.compile(aw + '.*?' + digit + '(.*)')
            for p in [p1, p2]:
                m = p.match(temporal)
                if m:
                    # changed by Cong Liu
                    # e.g. within 5 years
                    # min = -5
                    # max = 0
                    min_value = '-' + m.group(1)
                    max_value = '0'
                    value_unit = m.group(2)
                    value_unit = value_unit.strip()
                    if len(value_unit.split()) == 1:
                        p = re.compile('(' + '|'.join(unit) + ')')
                        m = p.match(value_unit)
                        if m:
                            value_unit = self.time2days[m.group(1)]
                            rate = rate2days[value_unit]
                            min_value = str(int(float(min_value) * rate))
                            return [min_value, max_value, value_unit]

        return [min_value, max_value, value_unit]


if __name__ == '__main__':
    """
    nlp_results = '../../resource/information_retrieval_results.txt'
    output = '../../resource/information_retrieval_results_plus.txt'
    p = Parser(nlp_results)
    p.parse_line(output)
    """
    # my_db = DbManager()
    # df = my_db.select_concept_ancestor(descendant_concept_id=3025395,min_levels_of_seperation=1)
    # print(df)
    input = '/Users/cl3720/Projects/DQueST/resource/concept_mapping_results.txt'
    blacklist = '/Users/cl3720/Projects/DQueST/resource/blacklist.csv'
    my_mapper = Mapper(input=input,blacklist=blacklist,min_levels_of_seperation=10)
    s_list = my_mapper.sorting_by_coverage()
    my_mapper.clustering_process(s_list)
