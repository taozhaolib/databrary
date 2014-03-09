define(['app/config/module'], function (module) {
	'use strict';

	module.filter('cite', ['$filter', function ($filter) {
		return function (volume) {
			if (!angular.isObject(volume) || angular.isUndefined(volume.access) || angular.isUndefined(volume.name) || angular.isUndefined(volume.id))
				return '';

			var names = [];

			angular.forEach(volume.access, function (access) {
				if (angular.isUndefined(access.access) || access.access < 4 /* FIXME: constant.find('permission', 'ADMIN').id */)
					return;

				var parts = access.party.name.split(' '),
					name = parts.pop();

				if (parts.length > 0) {
					name += ', ' + parts.map(function (n) {
						return n.charAt(0);
					}).join('. ') + '.';
				}

				names.push(name);
			});

			names = names.join(', ');

			//

			var created = $filter('date')(new Date(volume.creation), 'yyyy');
			var retrieved = $filter('date')(new Date(), 'longDate');

			//

			return names + ', ' + created + '. '+ volume.name + '. <em>Databrary</em>. Retrieved ' + retrieved + ' from <a href="http://databrary.org/volume/'+volume.id+'" title="'+ volume.name + '">http://databrary.org/volume/'+volume.id+'</a>.';
		};
	}]);
});
