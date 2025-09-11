# Decentralized Tutoring Marketplace
 
# Decentralized Tutoring Marketplace

A Clarity smart contract for connecting tutors and students using STX payments with escrow support.

## Features

- Tutor registration with customizable hourly rates
- Secure session booking with escrow
- Platform fee management
- Session completion and cancellation
- Tutor availability toggle

## Contract Functions

### For Tutors

- `register-as-tutor`: Register as a tutor with hourly rate
- `update-hourly-rate`: Update your hourly rate
- `toggle-availability`: Toggle your availability status
- `complete-session`: Mark a session as completed to receive payment

### For Students

- `book-session`: Book a session with a tutor
- `cancel-session`: Cancel a booked session

### Read-Only Functions

- `get-tutor`: Get tutor information
- `get-session`: Get session details

## Usage

1. Deploy the contract to the Stacks blockchain
2. Tutors register and set their rates
3. Students book sessions by sending STX
4. Tutors complete sessions to receive payment
5. Platform fee is automatically handled

## Platform Fees

Default platform fee is 5% (50 basis points). Only contract owner can modify this.