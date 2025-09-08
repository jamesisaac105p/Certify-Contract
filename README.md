# 🎓 Certify - On-Chain Academic Certificates

A blockchain-based certification system for issuing, managing, and verifying academic credentials on the Stacks blockchain.

## 🌟 Features

- 🏛️ **Institution Management**: Register and authorize educational institutions
- 📜 **Certificate Issuance**: Issue tamper-proof digital certificates
- ✅ **Verification**: Instantly verify certificate authenticity
- 🔒 **Revocation**: Revoke certificates when necessary
- 📊 **Tracking**: Track certificates by recipient or institution

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing


## 📖 Usage

### For Contract Owner

#### Register an Institution
```clarity
(contract-call? .certify register-institution "Harvard University" 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

#### Update Institution Authorization
```clarity
(contract-call? .certify update-institution-authorization u1 false)
```

### For Institution Admins

#### Issue a Certificate
```clarity
(contract-call? .certify issue-certificate 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7
  u1
  "Bachelor of Science"
  "Computer Science"
  u20240601
  (some u375)
  (some "Magna Cum Laude"))
```

#### Revoke a Certificate
```clarity
(contract-call? .certify revoke-certificate u1)
```

### For Anyone

#### Verify a Certificate
```clarity
(contract-call? .certify verify-certificate u1)
```

#### Get Certificate Details
```clarity
(contract-call? .certify get-certificate u1)
```

#### Get Institution Info
```clarity
(contract-call? .certify get-institution u1)
```

## 🔍 Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-certificate` | Get certificate details by ID |
| `get-institution` | Get institution details by ID |
| `verify-certificate` | Verify certificate validity |
| `get-recipient-certificate-count` | Get number of certificates for a recipient |
| `get-recipient-certificate-by-index` | Get recipient's certificate by index |
| `get-institution-certificate-count` | Get number of certificates issued by institution |
| `get-institution-certificate-by-index` | Get institution's certificate by index |
| `get-total-certificates` | Get total number of certificates issued |
| `get-total-institutions` | Get total number of registered institutions |

## 📊 Data Structure

### Certificate Fields
- **recipient**: Principal who earned the certificate
- **institution-id**: ID of issuing institution
- **degree-type**: Type of degree (e.g., "Bachelor of Science")
- **field-of-study**: Academic field (e.g., "Computer Science")
- **graduation-date**: Date of graduation (YYYYMMDD format)
- **gpa**: Grade point average (optional)
- **honors**: Academic honors (optional)
- **issued-at**: Block height when certificate was issued
- **revoked**: Whether certificate has been revoked

### Institution Fields
- **name**: Institution name
- **authorized**: Whether institution can issue certificates
- **admin**: Principal authorized to manage institution

## 🛡️ Security Features

- ✅ Only contract owner can register institutions
- ✅ Only institution admins can issue/revoke certificates
- ✅ Certificates are immutable once issued (except revocation status)
- ✅ All operations are logged on-chain
- ✅ Certificate verification is publicly accessible

## 🧪 Testing

```bash
clarinet test
```

## 📝 Error Codes

| Code | Description |
|------|-------------|
| u100 | Not authorized |
| u101 | Certificate not found |
| u102 | Certificate already exists |
| u103 | Invalid institution |
| u104 | Certificate revoked |

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.

---

