const express = require('express');
const path = require('path');

const app = express();

app.use(express.static(path.resolve('build')));
app.get('*', (req, res) => {
	res.sendFile(path.resolve('build', 'index.html'))
})

app.listen(process.env.PORT, (err) => {
	if (err) {
		throw err
	}
	console.log(`Server running on port: ${process.env.PORT}`)
})
