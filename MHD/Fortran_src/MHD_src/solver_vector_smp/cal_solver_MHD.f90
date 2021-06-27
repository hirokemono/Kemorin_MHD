!>@file   cal_solver_MHD.f90
!!@brief  module cal_solver_MHD
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2008
!!@date Modified in Nov., 2013
!
!>@brief  Wrapper for linear solvers for MHD dynmamo
!!
!!@verbatim
!!      subroutine solver_crank_vector(node, MG_param, num_MG_level,    &
!!     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,            &
!!     &          METHOD, PRECOND, eps, itr, i_field,                   &
!!     &          MG_vector, f_l, b_vec, nod_fld, x_vec, SR_sig, SR_r)
!!      subroutine solver_crank_scalar(node, MG_param, num_MG_level,    &
!!     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,            &
!!     &          METHOD, PRECOND, eps, itr, i_field,                   &
!!     &          MG_vector, f_l, b_vec, nod_fld, x_vec, SR_sig, SR_r)
!!      subroutine solver_poisson_scalar(node, MG_param, num_MG_level,  &
!!     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,            &
!!     &          METHOD, PRECOND, eps, itr, i_field,                   &
!!     &          MG_vector, f_l, b_vec, nod_fld, x_vec, SR_sig, SR_r)
!!        type(MGCG_parameter), intent(in) :: MG_param
!!        type(node_data), intent(in) :: node
!!        type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &                      :: MG_comm(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &                      :: MG_DJDS_tbl(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: MG_DJDS_mat(0:num_MG_level)
!!        type(finite_ele_mat_node), intent(in) :: f_l
!!        type(vectors_4_solver), intent(inout)                         &
!!       &                        :: MG_vector(0:num_MG_level)
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module cal_solver_MHD
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_interpolate_table
      use t_solver_djds
      use t_finite_element_mat
      use t_phys_data
      use t_phys_address
      use t_MGCG_parameter
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine solver_crank_vector(node, MG_param, num_MG_level,      &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,              &
     &          METHOD, PRECOND, eps, itr, i_field,                     &
     &          MG_vector, f_l, b_vec, nod_fld, x_vec, SR_sig, SR_r)
!
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer(kind = kint), intent(in) :: i_field
!
      type(MGCG_parameter), intent(in) :: MG_param
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(in) :: itr
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_DJDS_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_DJDS_mat(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                       :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(3*node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(3*node%numnod)
      type(phys_data), intent(inout) :: nod_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call copy_ff_to_rhs33                                             &
     &    (node%numnod, node%istack_nod_smp, f_l%ff, b_vec, x_vec)
!
      call solver_MGCG_vector(node, MG_param, num_MG_level,             &
     &    MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,                    &
     &    METHOD, PRECOND, eps, itr,  MG_vector, b_vec,                 &
     &    x_vec, SR_sig, SR_r)
!
      call copy_solver_vec_to_vector                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_field, x_vec, nod_fld%d_fld)
!
      end subroutine solver_crank_vector
!
!-----------------------------------------------------------------------
!
      subroutine solver_crank_scalar(node, MG_param, num_MG_level,      &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,              &
     &          METHOD, PRECOND, eps, itr, i_field,                     &
     &          MG_vector, f_l, b_vec, nod_fld, x_vec, SR_sig, SR_r)
!
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer(kind = kint), intent(in) :: i_field
!
      type(MGCG_parameter), intent(in) :: MG_param
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(in) :: itr
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_DJDS_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_DJDS_mat(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                       :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
      type(phys_data), intent(inout) :: nod_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call copy_ff_to_rhs11                                             &
     &   (node%numnod, node%istack_nod_smp, f_l%ff, b_vec, x_vec)
      call solver_MGCG_scalar(node, MG_param, num_MG_level,             &
     &    MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,                    &
     &    METHOD, PRECOND, eps, itr, MG_vector, b_vec,                  &
     &    x_vec, SR_sig, SR_r)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_field, x_vec, nod_fld%d_fld)
!
      end subroutine solver_crank_scalar
!
!-----------------------------------------------------------------------
!
      subroutine solver_poisson_scalar(node, MG_param, num_MG_level,    &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,              &
     &          METHOD, PRECOND, eps, itr, i_field,                     &
     &          MG_vector, f_l, b_vec, nod_fld, x_vec, SR_sig, SR_r)
!
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer(kind = kint), intent(in) :: i_field
!
      type(MGCG_parameter), intent(in) :: MG_param
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(in) :: itr
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_DJDS_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_DJDS_mat(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                        :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
      type(phys_data), intent(inout) :: nod_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call copy_ff_potential_to_rhs                                     &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_field, nod_fld%d_fld, f_l%ff, b_vec, x_vec)
      call solver_MGCG_scalar(node, MG_param, num_MG_level,             &
     &    MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,                    &
     &    METHOD, PRECOND, eps, itr, MG_vector, b_vec,                  &
     &    x_vec, SR_sig, SR_r)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_field, x_vec, nod_fld%d_fld)
!
      end subroutine solver_poisson_scalar
!
!-----------------------------------------------------------------------
!
      end module cal_solver_MHD
