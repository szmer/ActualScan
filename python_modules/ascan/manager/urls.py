from django.contrib.admin.views.decorators import staff_member_required
from django.urls import path

from . import views

urlpatterns = [
    path('sites/search/', views.SiteSearchList.as_view(), name='sites_search'),
    path('tags/search/', views.TagSearchList.as_view(), name='tags_search'),
    path('sites/', views.SiteList.as_view(), name='sites'),
    path('tags/', views.TagList.as_view(), name='tags'),
    path('site/<int:pk>/', views.site_details, name='site'),
    path('tag/<int:pk>/', views.tag_details, name='tag'),
    # this is an alias of the above, but using the primary key instead of the name
    path('tagname/<str:tag_name>/', views.tagname, name='tagname'),
    path('tagsites/<str:tag_name>/', views.tagsites, name='tagsites'),
    path('suggest/', views.suggest, name='suggest'),
    path('makesite/', views.makesite, name='makesite'),
    path('maketag/', views.maketag, name='maketag'),
    path('scaninfo/', views.scaninfo, name='scaninfo'),
    path('scans/', views.scanlist, name='scans'),
    path('suggestions/', views.suggestionlist, name='suggestions'),
    path('loadblocklist/', views.loadblocklist, name='loadblocklist'),
    path('decisions/', staff_member_required(views.SuggestionDecisions.as_view()), name='decisions'),
    path('decide_mod_comment/', views.decide_mod_comment, name='decide_mod_comment'),
    path('decide_status/', views.decide_status, name='decide_status'),
    path('decide_change/', views.decide_change, name='decide_change'),
]
