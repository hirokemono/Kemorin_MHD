!
!     module cal_solver_MHD
!
!        programmed by H.Matsui on June 2010
!
!!      subroutine cal_sol_velo_pre_crank
!!      subroutine cal_sol_mod_po
!!      subroutine cal_sol_vect_p_pre_crank
!!      subroutine cal_sol_magne_pre_crank
!!      subroutine cal_sol_mag_po
!!      subroutine cal_sol_energy_crank(i_fld)
!!      subroutine cal_sol_d_scalar_crank(i_fld)
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
      private :: copy_ff_to_rhs33, copy_ff_to_rhs11
      private :: copy_solver_vec_to_vector, copy_solver_vec_to_scalar
      private :: copy_ff_potential_to_rhs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_crank                                 &
     &         (node, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS,              &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,       &
     &          MG_mat_velo, MG_vector, i_velo, f_l, b_vec,             &
     &          x_vec, nod_fld)
!
      use solver_MGCG_MHD
!
      integer(kind = kint), intent(in) :: i_velo
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(DJDS_ordering_table), intent(in) :: DJDS_fluid
      type(DJDS_MATRIX), intent(in) :: Vmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm_fl(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_fl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_velo(0:num_MG_level)
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
     &    (node%numnod, node%istack_nod_smp, f_l%ff)
      call solver_MGCG_velo                                             &
     &    (node, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS,                   &
     &     num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,            &
     &    MG_mat_velo, MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_vector                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_velo, nod_fld%d_fld)
!
      end subroutine cal_sol_velo_pre_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_mod_po                                         &
     &         (node, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,               &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fll,      &
     &          MG_mat_press, MG_vector, i_p_phi, f_l, b_vec,           &
     &          x_vec, nod_fld)
!
      use solver_MGCG_MHD
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
     &    i_p_phi, nod_fld%d_fld, f_l%ff)
      call solver_MGCG_press                                            &
     &   (node, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,                     &
     &    num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fll,            &
     &    MG_mat_press, MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_p_phi, nod_fld%d_fld)
!
      end subroutine cal_sol_mod_po
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_crank                               &
     &         (node, iphys, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,     &
     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl,             &
     &          MG_mat_magne, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      use solver_MGCG_MHD
      use copy_nodal_fields
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(communication_table), intent(in) :: DJDS_comm_etr
      type(DJDS_ordering_table), intent(in) :: DJDS_entire
      type(DJDS_MATRIX), intent(in) :: Bmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_magne(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                      :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(3*node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(3*node%numnod)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_ff_to_rhs33                                             &
     &   (node%numnod, node%istack_nod_smp, f_l%ff)
      call solver_MGCG_magne                                            &
     &   (node, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,                  &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl,                   &
     &    MG_mat_magne, MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_vector                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    iphys%i_vecp, nod_fld%d_fld)
!
      call clear_nodal_data(node, nod_fld, n_scalar, iphys%i_m_phi)
!
      end subroutine cal_sol_vect_p_pre_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_magne_pre_crank                                &
     &         (node, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,            &
     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl,             &
     &          MG_mat_magne, MG_vector, i_magne, f_l, b_vec,           &
     &          x_vec, nod_fld)
!
      use solver_MGCG_MHD
!
      integer(kind = kint), intent(in) :: i_magne
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_etr
      type(DJDS_ordering_table), intent(in) :: DJDS_entire
      type(DJDS_MATRIX), intent(in) :: Bmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_magne(0:num_MG_level)
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(vectors_4_solver), intent(inout)                             &
     &                      :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(3*node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(3*node%numnod)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_ff_to_rhs33                                             &
     &   (node%numnod, node%istack_nod_smp, f_l%ff)
      call solver_MGCG_magne                                            &
     &   (node, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,                  &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl,                   &
     &    MG_mat_magne, MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_vector                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_magne, nod_fld%d_fld)
!
      end subroutine cal_sol_magne_pre_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_mag_po                                         &
     &         (node, DJDS_comm_etr, DJDS_linear, Fmat_DJDS,            &
     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl_l,           &
     &          MG_mat_magp, MG_vector, i_m_phi, f_l, b_vec,            &
     &          x_vec, nod_fld)
!
      use solver_MGCG_MHD
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
     &    i_m_phi, nod_fld%d_fld, f_l%ff)
      call solver_MGCG_magne_p                                          &
     &         (node, DJDS_comm_etr, DJDS_linear, Fmat_DJDS,            &
     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl_l,           &
     &          MG_mat_magp, MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_m_phi, nod_fld%d_fld)
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
      use solver_MGCG_MHD
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
     &   (node%numnod, node%istack_nod_smp, f_l%ff)
      call solver_MGCG_temp                                             &
     &   (node, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,                    &
     &    num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,             &
     &    MG_mat_temp, MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_fld, nod_fld%d_fld)
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
      use solver_MGCG_MHD
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
     &   (node%numnod, node%istack_nod_smp, f_l%ff)
      call solver_MGCG_d_scalar                                         &
     &   (node, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS,                    &
     &    num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,             &
     &    MG_mat_d_scalar, MG_vector, b_vec, x_vec)
!
      call copy_solver_vec_to_scalar                                    &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_fld, nod_fld%d_fld)
!
      end subroutine cal_sol_d_scalar_crank
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_ff_to_rhs33(numnod, inod_smp_stack, ff)
!
      use calypso_mpi
      use m_machine_parameter
      use m_array_for_send_recv
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ff(numnod,3)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          b_vec(3*inod-2) = ff(inod,1)
          b_vec(3*inod-1) = ff(inod,2)
          b_vec(3*inod  ) = ff(inod,3)
          x_vec(3*inod-2) = ff(inod,1)
          x_vec(3*inod-1) = ff(inod,2)
          x_vec(3*inod  ) = ff(inod,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ff_to_rhs33
!
!-----------------------------------------------------------------------
!
      subroutine copy_ff_to_rhs11(numnod, inod_smp_stack, ff)
!
      use calypso_mpi
      use m_machine_parameter
      use m_array_for_send_recv
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ff(numnod,1)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          b_vec(inod) = ff(inod,1)
          x_vec(inod) = ff(inod,1)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ff_to_rhs11
!
!-----------------------------------------------------------------------
!
      subroutine copy_ff_potential_to_rhs(numnod, inod_smp_stack,       &
     &           ncomp_nod, i_field, d_nod, ff)
!
      use m_machine_parameter
      use m_array_for_send_recv
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(in) :: ff(numnod,1)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          b_vec(inod) = ff(inod,1)
          x_vec(inod) = d_nod(inod,i_field)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ff_potential_to_rhs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_solver_vec_to_vector(numnod, inod_smp_stack,      &
     &          ncomp_nod, i_field, d_nod)
!
      use m_machine_parameter
      use m_array_for_send_recv
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          d_nod(inod,i_field  ) = x_vec(3*inod-2)
          d_nod(inod,i_field+1) = x_vec(3*inod-1)
          d_nod(inod,i_field+2) = x_vec(3*inod  )
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_solver_vec_to_vector
!
!-----------------------------------------------------------------------
!
      subroutine copy_solver_vec_to_scalar(numnod, inod_smp_stack,      &
     &          ncomp_nod, i_field, d_nod)
!
      use m_machine_parameter
      use m_array_for_send_recv
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          d_nod(inod,i_field  ) = x_vec(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_solver_vec_to_scalar
!
!-----------------------------------------------------------------------
!
      end module cal_solver_MHD
