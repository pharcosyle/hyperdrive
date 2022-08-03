var env = process.env.ENV;
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
        FunctionName: getBiome(env) + '-jumpgate',
        Payload: JSON.stringify(event)
    }).promise().then(res => JSON.parse(res.Payload));
};
