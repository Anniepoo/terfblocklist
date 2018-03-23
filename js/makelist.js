window.addEventListener('load', function() {
  const inputs = document.getElementsByClassName('userblock');
  let blocklist = new Set([]);
  for (var i = 0; i < inputs.length; i++) {
    blocklist.add(inputs[i].name);
    ((elt) =>
     elt.addEventListener('change', function() {
       const name = elt.name;
       blocklist.has(name) ? blocklist.delete(name) : blocklist.add(name);
     }))(inputs[i]);
  }
  document.getElementById('exportButton').addEventListener('click', function(ev) {
    ev.preventDefault();
    const csv = Array.from(blocklist).join("\n");
    const csvContent = "data:text/csv;charset=utf-8," + csv;
    let link = document.createElement('a');
    link.setAttribute('href', encodeURI(csvContent));
    link.setAttribute('download', 'terfblocker_blocklist.csv');
    document.body.appendChild(link);
    link.click();
  });
});
