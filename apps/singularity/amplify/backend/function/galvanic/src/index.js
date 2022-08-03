var env = process.env.ENV;

if (env === 'NONE') {
    var axios = require('axios');

    exports.handler = async function (event) {
        return axios.post('http://localhost:4000', event)
            .then(res => JSON.stringify(res.data))
            .catch(err => JSON.stringify({error: err}));
    };
} else {
    var aws = require('aws-sdk');
    var lambda = new aws.Lambda();

    function getBiome(env) {
        var biomeEnv = env;
        if (['dev', 'test'].includes(env))
            return 'biome-staging';
        return 'biome-krush'; // TODO supercollider
    }

    exports.handler = async function (event) {
        event.env = env;
        return lambda.invoke({
            FunctionName: getBiome(env) + '-warpgate',
            Payload: JSON.stringify(event)
        }).promise().then(res => res.Payload);
    }
}
