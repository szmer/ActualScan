import jQuery from '../node_modules/jquery/dist/jquery';
// export for others scripts to use
window.$ = window.jQuery = jQuery;

import 'modules/bootstrap';
import '../node_modules/bootstrap-select/js/bootstrap-select';
import '../node_modules/popper.js/dist/popper';

// We need to make the object public to make it accessible from inline scripts.
import Chartist from '../node_modules/chartist/dist/chartist';
window.Chartist = Chartist;
export default Chartist;

import moment from '../node_modules/moment/moment';
window.moment = moment;
