from searchfront.extensions import db

sites_tags = db.Table('sites_tags',
        db.Column('site_id', db.Integer(), db.ForeignKey('site.id')),
        db.Column('tag_id', db.Integer(), db.ForeignKey('tag.id')))

### Possible site types:
#    web
#    reddit

### Possible site/tag levels:
#    base
#    community
#    --- private not implemented

class Tag(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(256), nullable=False)
    description = db.Column(db.String(1024), nullable=False)
    level = db.Column(db.String(12), nullable=False)
    # (there's also a sites backref)

    def __repr__(self):
        """
        Implementing __repr__ is beneficial for displaying in forms.
        """
        return 'tag: \'{}\''.format(self.name)

# TODO subreddit sites and similar special cases
class Site(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    level = db.Column(db.String(12), nullable=False)
    homepage_url = db.Column(db.String(8192), nullable=False)
    # Homepage url w/o protocol and www, or /r/subreddit
    site_name = db.Column(db.String(512), nullable=False)
    # An url with search for "fat cat", represented as |||fat||| |||cat|||
    # For Reddit, the subreddit name without /r/
    search_pointer = db.Column(db.String(8192), nullable=False)
    source_type = db.Column(db.String(32), nullable=False)
    site_type = db.Column(db.String(32), nullable=False)
    tags = db.relationship('Tag', secondary=sites_tags,
            backref=db.backref('sites', lazy=True))

    MOCK_STR1 = '|||fat|||'
    MOCK_STR2 = '|||cat|||'

    def __repr__(self):
        """
        Implementing __repr__ is beneficial for displaying in forms.
        """
        return 'site on {}'.format(self.site_name)

    def search_url_for(self, tokens):
        """
        An url that should search for tokens on the site.
        """
        mock_idx1 = self.search_pointer.index(self.MOCK_STR1)
        mock_idx2 = self.search_pointer.index(self.MOCK_STR2)
        search_format = (self.search_pointer[:mock_idx1+len(self.MOCK_STR1)]
                + self.search_pointer[mock_idx2+len(self.MOCK_STR2):]).replace(self.MOCK_STR1, '{}')
        next_token_format = self.search_pointer[mock_idx2:mock_idx2+len(self.MOCK_STR2)].replace(
                self.MOCK_STR2, '{}')
        if len(tokens) <= 1:
            # Technically there shouldn't be zero tokens, but we shouldn't crash with this problem
            # now.
            return search_format.format(tokens[0] if tokens else '')
        else:
            for i in range(len(tokens)-1):
                search_format += next_token_format
            return search_format.format(*tokens)
