const stop = document.location.pathname.split("/")[2];
// alert(stop); 

function getDepartures() {
    fetch("/request?stop=" + stop)
    .then(response => {
        if (response.ok) return response.json();
        else throw new Error(response.status + " " + response.statusText);
    })
    .then(result => console.log(result))
    .catch(error => console.error(error));
}

function getDepartures2() {
    fetch("/request?error=" + stop)
    .then(response => {
        if (response.ok) return response.json();
        else throw new Error(response.status + " " + response.statusText);
    })
    .then(result => console.log(result))
    .catch(error => console.error(error));
}