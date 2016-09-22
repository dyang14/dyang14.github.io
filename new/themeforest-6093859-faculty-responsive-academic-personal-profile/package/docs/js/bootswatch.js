$('a[rel=tooltip]').tooltip({
	'placement': 'bottom'
});

$("pre").each(function(){
	var html = $(this).html();

	html = html.replace(/</g,'&lt;').replace("/>/g","&gt;");

	$(this).html(html);
});