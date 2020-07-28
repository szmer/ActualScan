from django.urls import path

from . import views

app_name = 'manager'

urlpatterns = [
    path('sites/', views.SiteList.as_view(), name='sites'),
    path('tags/', views.TagList.as_view(), name='tags'),
    path('site/<int:pk>/', views.SiteDetails.as_view(), name='site'),
    path('tag/<int:pk>/', views.TagDetails.as_view(), name='tag'),
    path('tagsites/<tag>/', views.TagSiteList.as_view(), name='tagsites'),
    path('makesite/', views.makesite, name='makesite'),
    path('maketag/', views.maketag, name='maketag'),
]
