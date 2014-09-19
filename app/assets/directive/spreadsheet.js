'use strict';

module.directive('spreadsheet', [
  'pageService', function (page) {
    var MAXLEN = 16; // maximum number of records per category per slot
    function maybeInt(s) {
      var i = parseInt(s);
      return isNaN(i) ? s : i;
    }
    function byNumber(a,b) { return a-b; }
    function byType(a,b) {
      var ta = typeof a;
      var tb = typeof b;
      if (ta > tb) return 1;
      if (ta < tb) return -1;
      return a>b?1:a<b?-1:0;
    }
    function byMagic(a,b) {
      var na = parseFloat(a);
      var nb = parseFloat(b);
      if (na > nb) return 1;
      if (na < nb) return -1;
      return byType(a,b);
    }

    function stripPrefix(s, prefix) {
      return s.startsWith(prefix) ? s.substr(prefix.length) : undefined;
    }

    /* autovivification */
    function arr(a, f) {
      if (f in a)
	return a[f];
      else
	return (a[f] = []);
    }

    function obj(a, f) {
      if (f in a)
	return a[f];
      else
	return (a[f] = {});
    }

    function inc(a, f) {
      if (f in a)
	return a[f] ++;
      else {
	a[f] = 1;
	return 0;
      }
    }

    function parseInfo(id) {
      var info = {};
      var s = id.split('_');
      if ((info.t = stripPrefix(s[0], 'ss-')) === undefined ||
	  s.length > 1 && isNaN(info.i = parseInt(s[1])))
	return;
      switch (info.t) {
	case 'rec':
	  info.n = parseInt(s[2]);
	  info.m = parseInt(s[3]);
	  break;
	case 'add':
	  info.c = parseInt(s[2]);
	  break;
	case 'metric':
	  info.m = info.i;
	  delete info.i;
	  break;
	case 'category':
	  info.c = info.i;
	  delete info.i;
	  break;
      }
      return info;
    }

    var noCategory = {
      id: 0,
      name: 'record',
      not: 'No record',
      template: [page.metric.ident.id]
    };
    Object.freeze(noCategory);

    function getCategory(c) {
      return c ? page.constants.category[c] : noCategory;
    }

    var pseudoMetrics = {
      id: {
	id: 'id',
	name: 'id',
	type: 'number',
	classification: page.classification.PUBLIC
      },
      age: {
	id: 'age',
	name: 'age',
	type: 'number',
	classification: page.classification.SHARED
      },
    };
    page.constants.deepFreeze(pseudoMetrics);

    function getMetric(m) {
      return pseudoMetrics[m] || page.constants.metric[m];
    }

    var controller = [
      '$scope', function ($scope) {

	var volume = $scope.volume;
	$scope.page = page;

	$scope.editable = volume.checkPermission(page.permission.EDIT);
	var editing = $scope.editing = false;

	function getSlot(slot) {
	  if ('records' in slot)
	    return slot;
	  angular.extend(slot, volume.containers[slot.id]);
	  if (slot.segment !== undefined) {
	    slot.records = slot.records.filter(function (rec) {
	      return page.types.segmentOverlaps(slot.segment, rec.segment);
	    });
	  }
	  return slot;
	}

	/*
	 * We use the following types of data structures:
	 *   Row = index of slot in slots and rows (i)
	 *   Data[Row] = scalar value (array over Row)
	 *   Slot_id = Database id of container
	 *   Segment = standard time range (see type service)
	 *   Record_id = Database id of record
	 *   Category_id = Database id of record category (c)
	 *   Count = index of record within category for slot (n)
	 *   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
	 */

	var slots = []; // [Row] = Slot
	angular.forEach(volume.containers, function (s) {
	  slots.push(/*getSlot*/(s));
	});

	var count = slots.length;
	var order = Object.keys(slots); // Permutation Array of Row in display order

	var records = {}; // [Category_id][Metric_id][Count] :: Data
	var counts = new Array(count); // [Row][Category_id] :: Count
	var recordCols = [], // [] Array over records :: {category: Category_id, metrics[]: Array of Metric_id}
	    metricCols = []; // [] Array over metrics :: {category: Category, metric: Metric} (flattened version of recordCols)
	var depends = {}; // [Record_id][Row] :: Count

	var rows = new Array(count); // [Row] :: DOM Element tr

	var table = document.getElementById('spreadsheet-table');

	/* may be called after parseInfo to fill out complete information */
	function fillInfo(info) {
	  if ('m' in info) {
	    info.c = (info.category = (info.col = metricCols[info.m]).category).id;
	    info.metric = info.col.metric;
	  } else if ('c' in info)
	    info.category = getCategory(info.c);
	  if ('i' in info) {
	    info.slot = slots[info.i];
	    if ('n' in info)
	      info.record = volume.records[info.r = records[info.c].id[info.n][info.i]];
	  }
	  return info;
	}

	///////////////////////////////// Populate data structures 

	/* Fill all Data values for Row i */
	function populateSlot(i) {
	  var slot = slots[i];
	  
	  var r, n;
	  function populateMeasure(m, v) {
	    arr(arr(r, m), n)[i] = v;
	  }
	  var count = counts[i] = {};

	  for (var ri = 0; ri < slot.records.length; ri ++) {
	    var record = volume.records[slot.records[ri].id];
	    var c = record.category || noCategory.id;

	    // populate depends:
	    if (record.id in depends) {
	      // skip duplicates:
	      if (i in depends[record.id])
		continue;
	    } else
	      depends[record.id] = {};

	    // populate records:
	    if (c in records)
	      r = records[c];
	    else
	      r = records[c] = {id: []};

	    // determine Count:
	    n = inc(count, c);

	    // populate measures:
	    populateMeasure('id', record.id);
	    if (!editing && 'age' in slot.records[ri])
	      populateMeasure('age', slot.records[ri].age);
	    for (var m in record.measures)
	      populateMeasure(m, record.measures[m]);

	    depends[record.id][i] = n;
	  }
	}

	/* Fill metricCols and recordCols from records */
	function populateCols() {
	  metricCols = [];
	  $scope.recordCols = recordCols = Object.keys(records).sort(byNumber).map(function (c) {
	    var category = getCategory(c);
	    if (editing)
	      category.template.forEach(function (m) {
		arr(records[c], m);
	      });
	    var metrics = Object.keys(records[c]).filter(function (m) {
	      // filter out 'id' and long metrics (e.g., Description)
	      return m !== 'id';
	    }).map(maybeInt);
	    if (metrics.length)
	      metrics.sort(byType);
	    else // add id if there's nothing else
	      metrics = ['id'];
	    var si = metricCols.length;
	    metricCols.push.apply(metricCols, metrics.map(function (m) {
	      return {
		category: category,
		metric: getMetric(m)
	      };
	    }));
	    var l = metrics.length;
	    metricCols[si].first = metricCols[si+l-1].last = l;
	    return {
	      category: category,
	      metrics: metrics
	    };
	  });
	  $scope.metricCols = metricCols;
	}

	/* Call all populate functions */
	function populate() {
	  records = {};
	  depends = {};
	  for (var i = 0; i < count; i ++)
	    populateSlot(i);
	  populateCols();
	  generate();
	}

	///////////////////////////////// Generate HTML
	
	/* Find the text content of cell c with element t */
	function setCell(c, t) {
	  var el = c.lastChild;
	  if (el && el.nodeType === 3)
	    c.replaceChild(t, el);
	  else
	    c.appendChild(t);
	}

	/* Add or replace the text contents of cell c for measure/type m with value v */
	function generateText(c, m, v, assumed) {
	  if (assumed)
	    c.classList.remove('assumed');
	  if (v === undefined)
	  {
	    if (assumed) {
	      v = assumed;
	      c.classList.add('assumed');
	    } else
	      v = '';
	  }
	  else if (m === 'age')
	    v = page.display.formatAge(v);
	  else if (m === 'consent') {
	    if (v in page.constants.consent) {
	      var cn = page.constants.consent[v];
	      c.className = cn + ' consent icon hint-consent-' + cn;
	      v = '';
	    }
	  } else if (typeof v === 'string' && v.length >= MAXLEN)
	    v = v.substr(0, MAXLEN) + '...';
	  setCell(c, document.createTextNode(v));
	}

	/* Add a td element to tr r with value c and id i */
	function generateCell(r, m, v, i, assumed) {
	  var td = r.appendChild(document.createElement('td'));
	  if (v === null)
	    td.className = 'null';
	  else {
	    generateText(td, m, v, assumed);
	    td.id = i;
	  }
	  return td;
	}

	function generateAdd(r, i, c, l) {
	  var td = r.appendChild(document.createElement('td'));
	  td.setAttribute("colspan", l);
	  td.appendChild(document.createTextNode("add " + c.name));
	  td.id = 'ss-add_' + i + '_' + c.id;
	  td.className = 'add';
	}

	/* Add all the record/measure tds to row i for count n */
	function generateRecords(row, i, n, edit) {
	  var count = counts[i];
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var col = metricCols[mi];
	    var c = col.category.id;
	    var m = col.metric.id;
	    var t = count[c] || 0;
	    var v;
	    if (edit && n === t) {
	      if (col.first)
		generateAdd(row, i, col.category, col.first);
	      continue;
	    }
	    if (n >= t)
	      v = null;
	    else
	      v = records[c][m][n] && records[c][m][n][i];
	    var cell = generateCell(row, m, v, 'ss-rec_' + i + '_' + n + '_' + mi, col.metric.assumed);
	    if (v !== null) {
	      var ri = 'ss-rec_' + records[c].id[n][i];
	      cell.classList.add(ri, ri + '_' + m);
	    }
	    if (col.first && n === 0 && t > 1)
	      cell.classList.add('more');
	  }
	}

	/* Fill out rows[i]. */
	function generateRow(i) {
	  var slot = slots[i];
	  var top = editing && slot.id === volume.top.id;
	  var row = document.createElement('tr');
	  if (rows[i] && rows[i].parentNode)
	    rows[i].parentNode.replaceChild(row, rows[i]);
	  rows[i] = row;
	  var cell;
	  row.id = 'ss_' + i;
	  row.data = i;
	  if (top)
	    row.className = 'top';

	  cell = generateCell(row, 'name', slot.name, 'ss-name_' + i);
	  cell = cell.insertBefore(document.createElement('a'), cell.firstChild);
	  cell.setAttribute('href', slot.route);
	  cell.classList.add('link', 'icon');

	  generateCell(row, 'date', slot.date, 'ss-date_' + i);
	  generateCell(row, 'consent', slot.consent, 'ss-consent_' + i);
	  generateRecords(row, i, 0, editing && !top);
	}

	/* Update all age displays. */
	function regenerateAges() {
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var m = metricCols[mi];
	    if (m.metric.id !== 'age')
	      continue;
	    var c = m.category.id;
	    var r = records[c][m.metric.id][0];
	    var post = '_0_' + mi;
	    for (var i = 0; i < count; i ++) {
	      if (counts[i][c] && r)
		generateText(
		  document.getElementById('ss-rec_' + i + post),
		  'age', r[i]);
	    }
	    if (expanded !== undefined)
	      r = records[c][m.metric.id];
	      for (var n = 0; n < counts[expanded][c]; n ++) {
		if (n in r) generateText(
		    document.getElementById('ss-rec_' + expanded + '_' + n + '_' + mi),
		    'age', r[n][expanded]);
	      }
	  }
	}

	page.events.listen($scope, 'displayService-toggleAge', regenerateAges);

	/* Generate all rows. */
	function generate() {
	  for (var i = 0; i < count; i ++)
	    generateRow(i);
	  fill();
	}

	///////////////////////////////// Place DOM elements
	
	var tbody_main = document.getElementById('ss');
	var tbody_top = document.getElementById('ss-top');

	function tbody(i) {
	  return slots[i].top ? tbody_top : tbody_main;
	}

	/* Place all rows into spreadsheet. */
	function fill() {
	  collapse();
	  for (var o = 0; o < count; o ++) {
	    var i = order[o];
	    tbody(i).appendChild(rows[i]);
	  }
	}

	/* Populate order based on compare function applied to values. */
	function sort(values, compare) {
	  if (!values)
	    return;
	  if (!compare)
	    compare = byMagic;
	  order.sort(function (i, j) {
	    return compare(values[i], values[j]);
	  });
	}

	var currentSort;

	/* Sort by values, called name. */
	function sortBy(name, values) {
	  if (currentSort === name)
	    order.reverse();
	  else {
	    sort(values);
	    currentSort = name;
	  }
	  fill();
	}

	/* Sort by one of the container columns. */
	function sortBySlot(f) {
	  sortBy(f, slots.map(function (s) { return s[f]; }));
	}

	/* Sort by Category_id c's Metric_id m */
	function sortByMetric(c, m) {
	  sortBy(c + '_' + m, records[c][m][0]);
	}

	///////////////////////////////// Backend saving
	
	function saveError(cell, res) {
	  cell.classList.remove('saving');
	  cell.classList.add('error');
	  page.messages.addError({
	    body: 'error',
	    report: res
	  });
	}

	function createSlot(cell, top) {
	  cell.classList.add('saving');
	  volume.createContainer({top:top}).then(function (slot) {
	    if (!('records' in slot))
	      slot.records = [];
	    var i = (count = slots.push(slot))-1;
	    order.push(i);
	    populateSlot(i);
	    generateRow(i);
	    tbody(i).appendChild(rows[i]);
	    cell.classList.remove('saving');
	  }, saveError.bind(null, cell));
	}

	function saveSlot(cell, info, v) {
	  var data = {};
	  data[info.t] = v === undefined ? '' : v;
	  cell.classList.add('saving');
	  return info.slot.save(data).then(function () {
	    generateText(cell, info.t, v);
	    cell.classList.remove('saving');
	  }, saveError.bind(null, cell));
	}

	function saveMeasure(cell, info, v) {
	  cell.classList.add('saving');
	  return info.record.measureSet(info.metric.id, v).then(function (rec) {
	    var rcm = records[rec.category][info.metric.id];
	    angular.forEach(depends[info.r], function (n, i) {
	      arr(rcm, n)[i] = v;
	      /* TODO age may have changed... not clear how to update. */
	    });

	    var l = table.getElementsByClassName('ss-rec_' + info.r + '_' + info.metric.id);
	    for (var li = 0; li < l.length; li ++)
	      generateText(l[li], info.metric.id, v, info.metric.assumed);
	    cell.classList.remove('saving');
	  }, saveError.bind(null, cell));
	}

	function setRecord(cell, info, record) {
	  cell.classList.add('saving');
	  var act;
	  if (info.record)
	    act = info.slot.removeRecord(info.record)
	      .then(function () {
		return record && info.slot.addRecord(record);
	      });
	  else if (record)
	    act = info.slot.addRecord(record);
	  else
	    act = info.slot.newRecord(info.c);

	  return act.then(function (record) {
	    var r, m, rcm;
	    if (record) {
	      r = record.id;
	      if (!('n' in info))
		info.n = inc(counts[info.i], info.c);

	      for (m in records[info.c]) {
		rcm = records[info.c][m];
		var v = m in record ? record[m] : record.measures[m];
		if (v === undefined) {
		  if (info.n in rcm)
		    delete rcm[info.n][info.i];
		} else
		  arr(rcm, info.n)[info.i] = v;
		/* TODO age? maybe server should send it? */
	      }
	    } else {
	      var t = --counts[info.i][info.c];
	      for (m in records[info.c]) {
		rcm = records[info.c][m];
		for (var n = info.n+1; n < rcm.length; n ++)
		  arr(rcm, n-1)[info.i] = arr(rcm, n)[info.i];
		if (t in rcm)
		  delete rcm[t][info.i];
	      }
	    }

	    if (info.record)
	      delete depends[info.r][info.i];
	    if (record)
	      obj(depends, r)[info.i] = info.n;

	    cell.classList.remove('saving');
	    collapse();
	    if (info.n === 0)
	      generateRow(info.i);
	    expand(info.i);
	  }, saveError.bind(null, cell));
	}

	///////////////////////////////// Interaction
	
	var expanded;

	/* Collapse any expanded row. */
	function collapse() {
	  if (expanded === undefined)
	    return;
	  var i = expanded;
	  expanded = undefined;
	  var row = rows[i];
	  var el;
	  var p = row.parentNode; // tbody(i)
	  if (!((el = row.nextSibling) && el.data === i))
	    return false;
	  do {
	    p.removeChild(el);
	  } while ((el = row.nextSibling) && el.data === i);

	  for (el = row.firstChild; el && !el.id.startsWith("ss-rec_"); el = el.nextSibling)
	    el.removeAttribute("rowspan");
	  return true;
	}

	/* Expand (or collapse) a row */
	function expand(i) {
	  var row = rows[i];

	  if (expanded === i)
	    return;
	  collapse();
	  expanded = i;

	  var max = 0;
	  for (var c in counts[i])
	    if (counts[i][c] > max)
	      max = counts[i][c];
	  var edit = editing && slots[i].id !== volume.top.id;
	  if (edit)
	    max ++;
	  if (max <= 1)
	    return;
	  var next = row.nextSibling;
	  var el;
	  var p = tbody(i);
	  for (var n = 1; n < max; n ++) {
	    el = p.insertBefore(document.createElement('tr'), next);
	    el.data = i;
	    generateRecords(el, i, n, edit);
	  }

	  for (el = row.firstChild; !el.id.startsWith("ss-rec_"); el = el.nextSibling)
	    el.setAttribute("rowspan", max);
	}

	function save(cell, type, value) {
	  var info = fillInfo(parseInfo(cell.id));
	  if (value === '')
	    value = undefined;
	  else switch (type) {
	    case 'date':
	      value = page.$filter('date')(value, 'yyyy-MM-dd');
	      break;
	    case 'number':
	      value = parseFloat(value);
	      break;
	    case 'consent':
	      value = parseInt(value);
	      break;
	    case 'record':
	      if (value === 'new')
		setRecord(cell, info);
	      else if (value === 'remove')
		setRecord(cell, info);
	      else if (value !== undefined) {
		var ri = parseInt(value);
		if (ri !== info.r)
		  setRecord(cell, info, volume.records[ri]);
		else
		  edit(cell, info, true);
	      }
	      return;
	    case 'metric':
	      if (value !== undefined) {
		arr(records[info.c], value);
		populateCols();
		generate();
	      }
	      return;
	    case 'category':
	      if (value !== undefined) {
		arr(obj(records, value), 'id');
		populateCols();
		generate();
	      }
	      return;
	  }

	  switch (info.t) {
	    case 'name':
	    case 'date':
	    case 'consent':
	      return saveSlot(cell, info, value);
	    case 'rec':
	      return saveMeasure(cell, info, value);
	  }
	}

	var editScope = $scope.$new(true);
	editScope.page = page;
	var editInput = editScope.input = {};
	var editCellTemplate = page.$compile(page.$templateCache.get('spreadsheetEditCell.html'));
	var editCell;

	function unedit(event) {
	  var edit;
	  if (!(edit = editCell))
	    return;
	  editCell = undefined;
	  $(edit).children('[name=edit]').off();
	  var cell = edit.parentNode;
	  if (!cell)
	    return;
	  cell.removeChild(edit);
	  cell.classList.remove('editing');
	  page.tooltips.clear();

	  if (event && event.type === 'change')
	    save(cell, editScope.type, editInput.value);
	}
	editScope.unedit = unedit;

	function edit(cell, info, alt) {
	  fillInfo(info);
	  if (info.slot && info.slot.id === volume.top.id)
	    return;
	  switch (info.t) {
	    case 'name':
	      editScope.type = 'text';
	      editInput.value = info.slot.name;
	      break;
	    case 'date':
	      editScope.type = 'date';
	      editInput.value = info.slot.date;
	      break;
	    case 'consent':
	      editScope.type = 'consent';
	      editInput.value = info.slot.consent + '';
	      break;
	    case 'rec': if (!info.col.first || alt) {
	      var m = info.metric.id;
	      /* we need a real metric here: */
	      if (typeof m !== 'number')
		return;
	      editInput.value = volume.records[info.r].measures[m];
	      if (info.metric.options) {
		editScope.type = 'select';
		editScope.options = [''].concat(info.metric.options);
	      } else if (info.metric.long)
		editScope.type = 'long';
	      else
		editScope.type = info.metric.type;
	      break;
	    }
	      /* falls through */
	    case 'add':
	      var c = info.category;
	      if ('r' in info)
		editInput.value = info.r + '';
	      else
		editInput.value = 'remove';
	      editScope.type = 'record';
	      editScope.options = {
		'new': 'Create new ' + c.name,
		'remove': c.not
	      };
	      angular.forEach(volume.records, function (r, ri) {
		if ((r.category || 0) === c.id && (!(ri in depends && info.i in depends[ri]) || ri === editInput.value))
		  editScope.options[ri] = r.displayName;
	      });
	      break;
	    case 'category':
	      editScope.type = 'metric';
	      editInput.value = undefined;
	      editScope.options = {};
	      angular.forEach(page.constants.metric, function (m, mi) {
		if (!(mi in records[info.c]))
		  editScope.options[mi] = m.name;
	      });
	      break;
	    case 'head':
	      editScope.type = 'category';
	      editInput.value = undefined;
	      editScope.options = {};
	      angular.forEach(page.constants.category, function (c, ci) {
		if (!(ci in records))
		  editScope.options[ci] = c.name;
	      });
	      editScope.options[noCategory.id] = 'other';
	      break;
	    default:
	      return;
	  }
	  var e = editCellTemplate(editScope, function (e) {
	    cell.insertBefore(editCell = e[0], cell.firstChild);
	    cell.classList.add('editing');
	  });

	  page.tooltips.clear();
	  page.$timeout(function () {
	    var input = e.children('[name=edit]');
	    input.focus();
	    input.change(function (event) {
	      return $scope.$apply(function () {
		return unedit(event);
	      });
	    });
	  });
	}

	var selectStyles = document.head.appendChild(document.createElement('style')).sheet;

	function unselect() {
	  while (selectStyles.cssRules.length)
	    selectStyles.deleteRule(0);

	  unedit();
	}

	function select(cell, info) {
	  unselect();

	  expand(info.i);

	  if (info.t === 'rec') {
	    var cl = cell.classList;
	    for (var ci = 0; ci < cl.length; ci ++) {
	      var c = cl[ci];
	      if (c.startsWith('ss-rec_'))
		selectStyles.insertRule('.' + c + '{background-color:' +
		  (!c.contains('_', 7) ? '#ffff88' : '#88ff88') + 
		  ';\n}', selectStyles.cssRules.length);
	    }
	  }

	  if (editing)
	    edit(cell, info);
	}

	$scope.click = function (event) {
	  var el = event.target;
	  if (el.tagName !== 'TD')
	    return;
	  var info = parseInfo(el.id);
	  if (!info)
	    return;

	  select(el, info);
	  if ('m' in info && metricCols[info.m].metric.id === 'age')
	    page.display.toggleAge();
	};

	$scope.clickSession = function ($event) {
	  unselect();
	  if (editing)
	    edit($event.target, {t:'head'});
	};
	$scope.clickSlot = sortBySlot;
	$scope.clickCategory = function ($event, col) {
	  unselect();
	  if (editing)
	    edit($event.target, {t:'category',c:col.category.id});
	};
	$scope.clickMetric = function (col) {
	  sortByMetric(col.category.id, col.metric.id);
	};
	$scope.clickNew = function ($event, top) {
	  createSlot($event.target, top);
	};

	///////////////////////////////// main

	$scope.refresh = function(e) {
	  unedit();
	  collapse();
	  if (e === undefined)
	    e = editing;
	  $scope.editing = editing = e;
	  populate();
	};

	populate();
      }
    ];

    return {
      restrict: 'E',
      scope: {
	volume: '=',
      },
      templateUrl: 'spreadsheet.html',
      controller: controller,
    };
  }
]);
