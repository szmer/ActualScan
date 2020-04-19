from flask_security import current_user

from searchfront.extensions import db

def current_user_id():
    if hasattr(current_user, 'id') and current_user.id:
        return current_user.id
    return None

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
    name = db.Column(db.String(256), nullable=False, unique=True)
    description = db.Column(db.String(1024))
    level = db.Column(db.Integer, nullable=False, default=10)
    creator_id = db.Column(db.Integer, db.ForeignKey('app_user.id'), default=current_user_id)
    # (there's also a sites backref)

    def __repr__(self):
        """
        Implementing __repr__ is beneficial for displaying in forms.
        """
        return '/{}/'.format(self.name)

class Site(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    level = db.Column(db.Integer, nullable=False, default=10)
    homepage_url = db.Column(db.String(8192), nullable=False, unique=True)
    # Homepage url w/o protocol and www, or /r/subreddit
    site_name = db.Column(db.String(512), nullable=False, unique=True)
    # An url with search for "fat cat", represented as |||fat||| |||cat|||
    # For Reddit, the subreddit name without /r/
    search_pointer = db.Column(db.String(8192), nullable=False)
    source_type = db.Column(db.String(32), nullable=False)
    site_type = db.Column(db.String(32), nullable=False)
    tags = db.relationship('Tag', secondary=sites_tags,
            backref=db.backref('sites', lazy=True))
    creator_id = db.Column(db.Integer, db.ForeignKey('app_user.id'), default=current_user_id)

    # NOTE we should take urlencoding schemes into account
    MOCK_STR1 = '|||fat|||'
    MOCK_STR2 = '|||cat|||'

    def __repr__(self):
        """
        Implementing __repr__ is beneficial for displaying in forms.
        """
        return 'site: {}'.format(self.site_name)

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
