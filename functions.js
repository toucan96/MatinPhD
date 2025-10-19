show = function(id) {
	var allDiv = document.getElementsByClassName('content');
	for (var i = 0; i < allDiv.length; i++) {
		allDiv[i].style.display = 'none'
	}
	var div = document.getElementById(id);
	div.style.display='block';
	var allA = document.querySelectorAll('.navbar li a');
	for (var n = 0; n < allA.length; n++) {
		allA[n].style.backgroundColor = ''
		allA[n].style.color = ''
	}
	var a = document.getElementById(id.concat('-btn'))
	a.style.backgroundColor='#5d5e5f';
	a.style.color='#ffffff'
}

load = function() {
	var start = location.hash.slice(1);
	if(start) {show(start)} else{show('home')}
}
