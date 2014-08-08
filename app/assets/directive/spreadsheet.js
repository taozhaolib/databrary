'use strict';

/* Remember, angular.forEach is 8-10x slower: http://jsperf.com/angularloops */

module.directive('spreadsheet', [
  'pageService', function (page) {
    var MAXLEN = 32; // maximum number of records per category per slot
    var byNumber = function(a,b) { return a-b; };
    var byString = function(a,b) { return a>b?1:a<b?-1:0; };
    var byType = function(a,b) {
      var ta = typeof a;
      var tb = typeof b;
      if (ta > tb) return 1;
      if (ta < tb) return -1;
      if (ta == 'number') return a-b;
      return a>b?1:a<b?-1:0;
    };

    var controller = [
      '$scope', function ($scope) {

	var volume = $scope.volume;

	var getSlot = function (slot) {
	  if ('records' in slot)
	    return slot;
	  angular.extend(slot, volume.sessions[slot.id]);
	  if (!angular.isUndefined(slot.segment)) {
	    slot.records = slot.records.filter(function (rec) {
	      return page.types.segmentOverlaps(slot.segment, rec.segment);
	    });
	  }
	  return slot;
	};

	/*
	 * We use the following types of data structures:
	 *   Row = index of slot in slots and rows
	 *   Data[Row] = scalar value (array over Row)
	 *   Slot_id = Database id of container
	 *   Segment = standard time range (see type service)
	 *   Record_id = Database id of record
	 *   Category_id = Database id of record category
	 *   Count = index of record within category for slot
	 *   Metric_id = Database id of metric, or "id" for Record_id, or "age"
	 */

	var slots = []; // [Row] = Slot
	angular.forEach(volume.sessions, function (s) {
	  slots.push(/*getSlot*/(s));
	});

	var count = slots.length;
	var order = Object.keys(slots); // Permutation Array of Row in display order

	var meta = {
	  id: new Array(count), // :: Data of Record_id
	  segment: new Array(count), // :: Data of Segment
	  date: new Array(count), // :: Data of container.date
	  top: new Array(count), // :: Data of container.top
	  consent: new Array(count), // :: Data of consent (entire slot only)
	};
	var records = {}; // [Category_id][Metric_id][Count] :: Data
	var counts = new Array(count); // [Row][Category_id] :: Count
	var recordCols = [], // [] Array over records :: {category: Category_id, metrics[]: Array of Metric_id}
	    metricCols = []; // [] Array over metrics :: {category: Category_id, metric: Metric_id} (flattened version of recordCols)
	var depends = {}; // [Record_id] :: Array of Row

	var rows = new Array(count); // [Row] :: DOM Element tr

	var ss = document.getElementById('ss');

	/* Fill all Data values for Row i */
	var populateSlot = function (i) {
	  var slot = slots[i];
	  
	  // populate meta:
	  meta.id[i] = slot.id;
	  meta.segment[i] = slot.segment;
	  meta.date[i] = slot.date;
	  meta.top[i] = slot.top;
	  meta.consent[i] = slot.consent;

	  var r, c;
	  var populateMeasure = function (m, v) {
	    if (!(m in r))
	      r[m] = [];
	    if (!(c in r[m]))
	      r[m][c] = [];
	    r[m][c][i] = v;
	  };
	  var count = counts[i] = {};

	  for (var ri = 0; ri < slot.records.length; ri ++) {
	    var record = volume.records[slot.records[ri].id];
	    var cat = record.category;

	    // populate records:
	    if (!(cat in records)) {
	      r = records[cat] = {id: []};
	      if (cat === page.category.participant.id)
		r.age = [];
	    } else
	      r = records[cat];

	    // determine Count:
	    if (!(cat in count)) {
	      count[cat] = 1;
	      c = 0;
	    } else
	      c = count[cat] ++;

	    // populate measures:
	    populateMeasure('id', record.id);
	    if ('age' in r)
	      populateMeasure('age', slot.records[ri].age);
	    for (var m in record.measures)
	      populateMeasure(m, record.measures[m]);

	    // populate depends:
	    if (!(record.id in depends))
	      depends[record.id] = [i];
	    else
	      depends[record.id].push(i);
	  }
	};

	/* Fill metricCols and recordCols from records */
	var populateCols = function () {
	  metricCols = [];
	  recordCols = Object.keys(records).sort(byNumber).map(function (c) {
	    var metrics = Object.keys(records[c]).filter(function (m) {
	      // filter out 'id' and long metrics (e.g., Description)
	      return m !== 'id' && !(m in page.constants.data.metric && page.constants.data.metric[m].long);
	    });
	    if (metrics.length)
	      metrics.sort(byNumber);
	    else // add id if there's nothing else
	      metrics = ['id'];
	    metricCols.push.apply(metricCols, metrics.map(function (metric) {
	      return {
		category: c,
		metric: metric
	      };
	    }));
	    return {
	      category: c,
	      metrics: metrics
	    };
	  });
	};

	/* Call all populate* functions */
	var populate = function () {
	  for (var i = 0; i < count; i ++)
	    populateSlot(i);
	  populateCols();
	};

	/* Add a td element to tr r with value c and id i */
	var generateCell = function (r, v, i) {
	  var td = r.appendChild(document.createElement('td'));
	  if (!angular.isUndefined(v)) {
	    if (typeof v === 'string' && v.length >= MAXLEN)
	      v = v.substr(0, MAXLEN) + '...';
	    td.appendChild(document.createTextNode(v));
	  }
	  td.id = i;
	  return td;
	};
	
	/* Fill out rows[i]. Should only be called once. */
	var generateRow = function (i) {
	  var row = rows[i] = document.createElement('tr');
	  row.id = 'ss-row:' + i;
	  row.data = i;
	  if (meta.top[i])
	    row.classList.add('ss-top');
	  generateCell(row, meta.date[i], 'ss-meta:date:' + i);
	  var consentName = page.constants.data.consent[meta.consent[i]];
	  var consent = generateCell(row, consentName, 'ss-meta:consent:' + i);
	  if (consentName) {
	    consent.classList.add(consentName.toLowerCase());
	    consent.setAttribute('hint', 'consent-' + consentName);
	  }
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var col = metricCols[mi];
	    var c = col.category;
	    var m = col.metric;
	    var v;
	    if (m === 'count')
	      v = counts[i][c];
	    else {
	      v = records[c][m][0][i];
	      if (m === 'age')
		v = page.display.formatAge(v);
	    }
	    generateCell(row, v, 'ss-rec:' + c + ':' + m + ':' + i);
	  }
	};

	var regenerateAge = function (id, v) {
	  if (!angular.isUndefined(v))
	    document.getElementById('ss-rec:' + id).firstChild.replaceWholeText(page.display.formatAge(v));
	};

	/* Update all age displays. */
	var regenerateAges = function () {
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var m = metricCols[mi];
	    if (m.metric !== 'age')
	      continue;
	    for (var i = 0; i < count; i ++)
	      regenerateAge(m.record + ':age:' + i, records[m.record][m.metric][0][i]);
	    if (expanded !== null) {
	      for (var n = 1; n < counts[expanded][m.category]; n ++)
		regenerateAge(m.record + ':age:' + expanded + ':' + n,
		   records[m.record][m.metric][n][expanded]);
	    }
	  }
	};

	page.events.listen($scope, 'displayService-toggleAge', regenerateAges);

	/* Generate all rows. Should only be called once. */
	var generate = function () {
	  for (var i = 0; i < count; i ++)
	    generateRow(i);
	};

	/* Place all rows into spreadsheet. */
	var fill = function () {
	  // appendChild removes as well
	  for (var i = 0; i < count; i ++)
	    ss.appendChild(rows[order[i]]);
	};

	/* Populate order based on compare function applied to values. */
	var sort = function (values, compare) {
	  if (!compare)
	    compare = byType;
	  order.sort(function (i, j) {
	    return compare(values[i], values[j]);
	  });
	};

	var currentSort;

	/* Sort by values, called name. */
	var sortBy = function (name, values) {
	  if (currentSort === name)
	    order.reverse();
	  else {
	    sort(values);
	    currentSort = name;
	  }
	  fill();
	};

	/* Sort by one of the container columns. */
	$scope.sortByMeta = function (f) {
	  sortBy('meta:' + f, meta[f]);
	};

	/* Sort by Category_id c's Metric_id m */
	$scope.sortByMetric = function (c, m) {
	  sortBy('metric:' + c + ':' + m, records[c][m][0]);
	};

	var expanded = null; // Row of currently expanded row
	var expansion = []; // Additional TRs needed by expansion

	/* Expand (or collapse) a row */
	var expand = function (row) {
	  var i = row.data;
	  if (angular.isUndefined(i))
	    return;

	  var cell;
	  if (expanded !== null) {
	    // collapse
	    for (cell = rows[expanded].firstChild; cell.id.lastIndexOf("ss-meta:", 0) === 0; cell = cell.nextSibling)
	      cell.removeAttribute("rowspan");
	    expansion.forEach(function (e) {
	      ss.removeChild(e);
	    });
	    expansion = [];
	  }
	  if (expanded === i) {
	    expanded = null;
	    return;
	  }

	  var count = counts[i];
	  var max = 0;
	  angular.forEach(count, function (n) {
	    max = Math.max(max, n);
	  });
	  var next = row.nextSibling;
	  for (var n = 1; n < max; n ++) {
	    var e = ss.insertBefore(document.createElement('tr'), next);

	    for (var mi = 0; mi < metricCols.length; mi ++) {
	      var col = metricCols[mi];
	      var c = col.category;
	      var m = col.metric;
	      var v;
	      if (m !== 'count' && n < count[c]) {
		v = records[c][m][n][i];
		if (m === 'age')
		  v = page.display.formatAge(v);
	      } else
		v = undefined;
	      generateCell(e, v, 'ss-rec:' + c + ':' + m + ':' + i + ':' + n);
	    }

	    expansion.push(e);
	  }

	  for (cell = row.firstChild; cell.id.lastIndexOf("ss-meta:", 0) === 0; cell = cell.nextSibling)
	    cell.setAttribute("rowspan", max);

	  expanded = i;
	};

	$scope.click = function (event) {
	  var cell = event.target;
	  if (cell.tagName !== 'TD')
	    return;
	  var row = cell.parentElement;
	  expand(row);
	};

	populate();
	generate();
	fill();
	$scope.recordCols = recordCols;
	$scope.metricCols = metricCols;
      }
    ];

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'spreadsheet.html',
      controller: controller,
    };
  }
]);
