define(['app/config/module'], function (module) {
	'use strict';

	module.filter('ident', ['$filter', function ($filter) {
		return function (object, volume, type) {
			type = ['session'].indexOf(type) > -1 ? type : 'session';

			switch(type) {
				case 'session':
					return 'Session '+object.container.id;
			}
		};
	}]);
});
