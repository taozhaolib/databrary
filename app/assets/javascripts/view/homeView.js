module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.display.title = page.constants.message('page.title.home');

		var lastName = function(s) {
			if(!s) return '';	
			return s.substr(s.lastIndexOf(' '));

		}

		angular.forEach(parties, function(v,k){
			v.lastName = lastName(v.name);
		});
		$scope.parties = page.$filter('toArray')(parties, 'lastName');
		$scope.volume = volume;

	}
]);
