'use strict';

module.directive('slotAnnotations', [
	'pageService', function (page) {
		var controller = [
			'$scope', '$element', '$attrs',
			function ($scope, $element, $attrs) {
				var ctrl = page.$parse($attrs.ctrl)($scope);
				var notes = this;
				notes.list = [];

				var $list, $head;

				notes.noteStyle = function (note) {
					var left = note.record.segment[0] / ctrl.clock.duration;
					var right = ((ctrl.clock.duration - note.record.segment[1]) / ctrl.clock.duration);

					return {
						'z-index': 500 - note.level,
						'top': 24 * note.level,
						'left': left ? left * 100 : 0,
						'right': right ? right * 100 : 0,
					};
				};

				var drawNote = function (levels, level, record) {
					levels[level].push(record);
					notes.list.push({
						record: record,
						level: level,
					});
				};

				var drawNotes = function (mode) {
					switch (mode) {
						case 'comments':
							// todo: comments
							$list.html('');
							$head.height(24);
							break;
						default:
							if (!ctrl.records[mode]) {
								ctrl.noteMode = null;
								$list.html('');
								$head.height(24);
								return;
							}

							ctrl.mode = page.constants.data.category[mode].name;

							var levels = [];

							angular.forEach(ctrl.records[mode], function (record) {
								if (!record.segment) {
									levels.push([record]);
									return;
								}

								// look for the first level that has enough space
								for (var i = 0, l = levels.length; i < l; i++) { // try existing levels
									var m = levels[i].length;

									// skip the loop if this level has a full-width record
									if (m === 1 && !levels[i][0].segment) {
										continue;
									}

									// if this record conflicts with any of this level, break
									for (var j = 0; j < m; j++) {
										// todo: review this logic
										if ((record.segment[0] && levels[i][j].segment[1] && levels[i][j].segment[1] >= record.segment[0]) || (record.segment[1] && levels[i][j].segment[0] && levels[i][j].segment[0] <= record.segment[1])) {
											break;
										}
									}

									// all good? then push
									drawNote(levels, i, record);
									return;
								}

								// no current level works? add a new one.
								levels.push([]);
								drawNote(levels, i, record);
							});
					}
				};

				page.$timeout(function () {
					$list = $element.find('slot-note-list');
					$head = $element.find('slot-note-head');
					$scope.$watch('ctrl.noteMode', drawNotes);
					drawNotes(ctrl.noteMode);
				});

				return notes;
			}];

		//

		return {
			restrict: 'E',
			scope: false,
			templateUrl: 'slotAnnotations.html',
			controller: controller,
			controllerAs: 'notes',
		};
	}
]);
