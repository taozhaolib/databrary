module.factory('CrossCite', [
	'resourceFactory', '$route', function (resource, $route) {
		return resource('http://data.crossref.org/:doi', {}, {
			apa: {
				method: 'GET',
				headers: {
					Accept: 'text/x-bibliography;style=apa',
				},
			},
			json: {
				method: 'GET',
				headers: {
					Accept: 'application/vnd.citationstyles.csl+json',
				},
			}
		}, 'crossCite');
	}
]);
