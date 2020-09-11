from django.urls import path

from . import views

app_name = 'manager'

urlpatterns = [
    path('sites/', views.SiteList.as_view(), name='sites'),
    path('tags/', views.TagList.as_view(), name='tags'),
    path('site/<int:pk>/', views.SiteDetails.as_view(), name='site'),
    path('tag/<int:pk>/', views.TagDetails.as_view(), name='tag'),
    # this is an alias of the above, but using the primary key instead of the name
    path('tagname/<str:tag_name>/', views.tagname, name='tagname'),
    path('tagsites/<str:tag_name>/', views.tagsites, name='tagsites'),
    path('makesite/', views.makesite, name='makesite'),
    path('maketag/', views.maketag, name='maketag'),
    path('scans/', views.scanlist, name='scans'),
]