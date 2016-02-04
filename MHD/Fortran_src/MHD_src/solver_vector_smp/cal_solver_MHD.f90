!
!     module cal_solver_MHD
!
!        programmed by H.Matsui on June 2010
!
!!      subroutine cal_sol_velo_pre_crank
!!     &         (node, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS,            &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,     &
!!     &          MG_mat_velo, MG_vector, i_velo, f_l, b_vec,           &
!!     &          x_vec, nod_fld)
!!      subroutine cal_sol_mod_po
!!     &         (node, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,             &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fll,    &
!!     &          MG_mat_press, MG_vector, i_p_phi, f_l, b_vec,         &
!!     &          x_vec, nod_fld)
!!      subroutine cal_sol_vect_p_pre_crank
!!     &         (node, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,          &
!!     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl,           &
!!     &          MG_mat_magne, MG_vector, i_vecp, f_l, b_vec,          &
!!     &          x_vec, nod_fld)
!!      subroutine cal_sol_magne_pre_crank
!!     &         (node, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,          &
!!     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl,           &
!!     &          MG_mat_magne, MG_vector, i_magne, f_l, b_vec,         &
!!     &          x_vec, nod_fld)
!!      subroutine cal_sol_mag_po
!!     &         (node, DJDS_comm_etr, DJDS_linear, Fmat_DJDS,          &
!!     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl_l,         &
!!     &          MG_mat_magp, MG_vector, i_m_phi, f_l, b_vec,          &
!!     &          x_vec, nod_fld)
!!      subroutine cal_sol_energy_crank
!!     &         (node, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,            &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,     &
!!     &          MG_mat_temp, MG_vector, i_fld, f_l, b_vec,            &
!!     &          x_vec, nod_fld)
!!      subroutine cal_sol_d_scalar_crank
!!     &         (node, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS,            &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,     &
!!     &          MG_mat_d_scalar, MG_vector, i_fld, f_l, b_vec,        &
!!     &          x_vec, nod_fld)
!
      module cal_solver_MHD
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_vector_for_solver
      use t_interpolate_table
      use t_solver_djds
      use t_finite_element_mat
      use t_phys_data
      use t_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine solver_crank_vector                                    &
     &         (node, DJDS_comm, DJDS_tbl, mat_DJDS, num_MG_level,      &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,              &
     &          METHOD, PRECOND, eps, itr, i_field,                     &
     &          MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer(kind = kint), intent(in) :: i_field
!
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(inout) :: itr
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm
      type(DJDS_ordering_table), intent(in) :: DJDS_tbl
      type(DJDS_MATRIX), intent(in) :: mat_DJDS
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
!
!
      call copy_ff_to_rhs33                                             &
     &    (node%numnod, node%istack_nod_smp, f_l%ff, b_vec, x_vec)
!
      call solver_MGCG_vector                                           &
     &   (node, DJDS_comm, DJDS_tbl, mat_DJDS, num_MG_level,            &
     &    MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,                    &
     &    METHOD, PRECOND, eps, itr,  MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_vector                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_field, x_vec, nod_fld%d_fld)
!
      end subroutine solver_crank_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_mod_po                                         &
     &         (node, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,               &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fll,      &
     &          MG_mat_press, MG_vector, i_p_phi, f_l, b_vec,           &
     &          x_vec, nod_fld)
!
      use m_iccg_parameter
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer(kind = kint), intent(in) :: i_p_phi
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(DJDS_ordering_table), intent(in) :: DJDS_fl_l
      type(DJDS_MATRIX), intent(in) :: Pmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm_fl(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_fll(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_press(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                        :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_ff_potential_to_rhs                                     &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_p_phi, nod_fld%d_fld, f_l%ff, b_vec, x_vec)
      call solver_MGCG_scalar                                           &
     &   (node, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS, num_MG_level,       &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fll, MG_mat_press,            &
     &    method_4_solver, precond_4_solver, eps, itr,                  &
     &    MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_p_phi, x_vec, nod_fld%d_fld)
!
      end subroutine cal_sol_mod_po
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_mag_po                                         &
     &         (node, DJDS_comm_etr, DJDS_linear, Fmat_DJDS,            &
     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl_l,           &
     &          MG_mat_magp, MG_vector, i_m_phi, f_l, b_vec,            &
     &          x_vec, nod_fld)
!
      use m_iccg_parameter
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer(kind = kint), intent(in) :: i_m_phi
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_etr
      type(DJDS_ordering_table), intent(in) :: DJDS_linear
      type(DJDS_MATRIX), intent(in) :: Fmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_l(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_magp(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                        :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_ff_potential_to_rhs                                     &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_m_phi, nod_fld%d_fld, f_l%ff, b_vec, x_vec)
      call solver_MGCG_scalar                                           &
     &   (node, DJDS_comm_etr, DJDS_linear, Fmat_DJDS, num_MG_level,    &
     &    MG_itp, MG_comm, MG_djds_tbl_l, MG_mat_magp,                  &
     &    method_4_solver, precond_4_solver, eps, itr,                  &
     &    MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_m_phi, x_vec, nod_fld%d_fld)
!
      end subroutine cal_sol_mag_po
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sol_energy_crank                                   &
     &         (node, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,              &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,       &
     &          MG_mat_temp, MG_vector, i_fld, f_l, b_vec,              &
     &          x_vec, nod_fld)
!
      use m_iccg_parameter
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer(kind = kint), intent(in) :: i_fld
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(DJDS_ordering_table), intent(in) :: DJDS_fluid
      type(DJDS_MATRIX), intent(in) :: Tmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm_fl(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_fl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_temp(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                       :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_ff_to_rhs11                                             &
     &   (node%numnod, node%istack_nod_smp, f_l%ff, b_vec, x_vec)
      call solver_MGCG_scalar                                           &
     &   (node, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS, num_MG_level,      &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, MG_mat_temp,              &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_fld, x_vec, nod_fld%d_fld)
!
      end subroutine cal_sol_energy_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_crank                                 &
     &         (node, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS,              &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,       &
     &          MG_mat_d_scalar, MG_vector, i_fld, f_l, b_vec,          &
     &          x_vec, nod_fld)
!
      use m_iccg_parameter
      use solver_MGCG_MHD
      use copy_for_MHD_solvers
!
      integer (kind = kint), intent(in) :: i_fld
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(DJDS_ordering_table), intent(in) :: DJDS_fluid
      type(DJDS_MATRIX), intent(in) :: Cmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm_fl(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_fl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_d_scalar(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                       :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_ff_to_rhs11                                             &
     &   (node%numnod, node%istack_nod_smp, f_l%ff, b_vec, x_vec)
      call solver_MGCG_scalar                                           &
     &   (node, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS, num_MG_level,      &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, MG_mat_d_scalar,          &
     &    method_4_solver, precond_4_solver, eps_4_comp_crank, itr,     &
     &    MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_fld, x_vec, nod_fld%d_fld)
!
      end subroutine cal_sol_d_scalar_crank
!
!-----------------------------------------------------------------------
!
      end module cal_solver_MHD
