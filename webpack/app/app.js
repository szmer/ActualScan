import 'modules/bootstrap';
import '../node_modules/bootstrap-select/js/bootstrap-select';

// We need to make the object public to make it accessible from inline scripts.
import Chartist from '../node_modules/chartist/dist/chartist';
window.Chartist = Chartist;
export default Chartist;
