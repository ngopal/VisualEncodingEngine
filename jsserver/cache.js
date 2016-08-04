function initializeCache() {
    if (sessionStorage.hasOwnProperty('beento') === false) {
        sessionStorage.setItem('beento', []);
        sessionStorage.setItem('togoto', d3.range(1, 37)); // 1, 36
        sessionStorage.setItem('tlength', 36); //need to add nodeconfig+edgeconfig length when ready
        sessionStorage.setItem('blength', 0);
        sessionStorage.setItem('status', 'incomplete');
        sessionStorage.setItem('consent', 'incomplete');
        sessionStorage.setItem('tutorial', 'incomplete');
        sessionStorage.setItem('demographic', 'incomplete');
        sessionStorage.setItem('thanks', 'incomplete');
    }
    else {
        if ( sessionStorage.getItem('consent') === 'incomplete' ) {
            window.location.href="/consent";
        }
        else {
            if ( sessionStorage.getItem('tutorial') === 'incomplete' ) {
                window.location.href="/tutorial";
            }
            else {
                if ( sessionStorage.getItem('status') === 'incomplete' ) {
                    //window.location.href="/";
                    console.log("experiment in progress");
                }
                else {
                    if ( sessionStorage.getItem('demographic') === 'incomplete' ) {
                        window.location.href="/demographic";
                    }
                    else {
                        if ( sessionStorage.getItem('thanks') === 'incomplete' ) {
                            window.location.href="/thanks";
                        }
                    }
                }
            }
        }
    }
}

function thissession(n) {
    initializeCache();

    if ( sessionStorage.getItem('status') === "complete" ) {
        alert('load next page');
    }

    var pageval = n;
    var beento = JSON.parse('['+sessionStorage.getItem('beento')+']');
    var togoto = JSON.parse('['+sessionStorage.getItem('togoto')+']');
    beento.push(pageval);
    togoto.splice(togoto.indexOf(parseInt(pageval)), 1);
    sessionStorage.setItem('beento', beento);
    sessionStorage.removeItem('togoto');
    sessionStorage.setItem('togoto', togoto);
    sessionStorage.setItem('tlength', togoto.length);
    sessionStorage.setItem('blength', beento.length);
    if (togoto.length === 0) {
        sessionStorage.setItem('status', 'complete');
        //sessionStorage.clear(); clear storage on exit page;
    }
};
