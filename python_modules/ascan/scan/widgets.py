from django.forms import DateInput

class MonthPickerInput(DateInput):
    template_name = 'widgets/monthpicker.html'
