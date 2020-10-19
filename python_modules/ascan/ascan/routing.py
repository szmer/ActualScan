from django.urls import path
from channels.routing import ProtocolTypeRouter, URLRouter
from channels.auth import AuthMiddlewareStack

import scan.consumers

application = ProtocolTypeRouter({
    "websocket": AuthMiddlewareStack(URLRouter([
        path("ws/rulesupdate/", scan.consumers.RulesUpdateConsumer),
        path("ws/scanprogr/", scan.consumers.ScanProgressConsumer),
        path("ws/tagfeedback/", scan.consumers.RelevanceFeedbackConsumer),
    ])),
})
