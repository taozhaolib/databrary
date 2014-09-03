module.controller('HomeView', [
	'$scope', 'investigators', 'users', 'volume', 'pageService',
	function ($scope, investigators, users, volume, page) {
		page.display.title = page.constants.message('page.title.home');

		var lastName = function (s) {
			return s ? s.substr(s.lastIndexOf(' ')+1) : '';
		}

		angular.forEach(investigators, function (v, k) {
			v.lastName = lastName(v.name);
		});
		angular.forEach(users, function (v, k) {
			v.lastName = lastName(v.name);
		});
		$scope.investigators = page.$filter('toArray')(investigators, 'lastName');
		$scope.users = page.$filter('toArray')(users, 'lastName');
		$scope.volume = volume;
	}
]);
