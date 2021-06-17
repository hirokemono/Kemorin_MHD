!>@file   t_mesh_SR.f90
!!@brief  module t_mesh_SR
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in June, 2021
!
!>@brief  Work area for data communications for mesh data
!!
!!@verbatim
!!@endverbatim
!!
      module t_mesh_SR
!
      use m_precision
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
      use t_vector_for_solver
!
      implicit none
!
!>      Structure of work area for mesh communications
      type mesh_SR
!>        Structure of communication flags
        type(send_recv_status) :: SR_sig
!>        Structure of communication buffer for 8-byte real
        type(send_recv_real_buffer) :: SR_r
!
!>        Structure of communication buffer for 4-byte integer
        type(send_recv_int_buffer) :: SR_i
!>        Structure of communication buffer for 8-byte integer
        type(send_recv_int8_buffer) :: SR_il
!
!>        Structure for vectors for solver
        type(vectors_4_solver) :: v_sol
      end type mesh_SR
!
!
      type(mesh_SR), save :: m_SR1
!
!
      end module t_mesh_SR
