/* eslint-disable */
// this is an auto generated file. This will be overwritten

export const baseDb = /* GraphQL */ `
  query BaseDb($freight: String) {
    baseDb(freight: $freight)
  }
`;
export const deltaTxs = /* GraphQL */ `
  query DeltaTxs($freight: String, $lastSync: AWSTimestamp) {
    deltaTxs(freight: $freight, lastSync: $lastSync)
  }
`;
