!cal_average_mag_potential.f90
!      module cal_average_mag_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine s_cal_average_mag_potential(FEM_prm, node, ele,      &
!!     &          iphys, nod_fld, inner_core, g_FEM, jac_3d_l)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_geometry_data), intent(in) :: inner_core
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d_l
!
      module cal_average_mag_potential
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
!
      implicit none
!
      real(kind=kreal), private :: ave_mp_core_local
      real(kind=kreal), private :: ave_mp_core
!
      private :: fem_icore_mag_potential_icore
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_average_mag_potential(FEM_prm, node, ele,        &
     &          iphys, nod_fld, inner_core, g_FEM, jac_3d_l)
!
      use calypso_mpi
      use calypso_mpi_real
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: inner_core
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_l
!
!
      if ( inner_core%numele_fld .eq. 0 ) return
!
        call fem_icore_mag_potential_icore                              &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      ele%interior_ele, inner_core%numele_fld,                    &
     &      inner_core%istack_ele_fld_smp, inner_core%iele_fld,         &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d_l%ntot_int, FEM_prm%npoint_t_evo_int,   &
     &      jac_3d_l%xjac, jac_3d_l%an,                                 &
     &      nod_fld%ntot_phys, nod_fld%d_fld, iphys%base%i_mag_p,       &
     &      ave_mp_core_local)
!
        call calypso_mpi_allreduce_one_real                             &
     &     (ave_mp_core_local, ave_mp_core, MPI_SUM)
!
        ave_mp_core = ave_mp_core * inner_core%volume
        if (my_rank.eq.0) write(84,*) ' ave_mp: ', ave_mp_core
!
      end subroutine s_cal_average_mag_potential
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_icore_mag_potential_icore                          &
     &         (numnod, numele, nnod_4_ele, ie, interior_ele,           &
     &          numele_in_core, iele_in_core_smp_stack, iele_in_core,   &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, an, ntot_phys, d_nod, i_mphi, &
     &          ave_mp_core_local)
!
      use m_machine_parameter
      use m_geometry_constants
!
      integer(kind=kint), intent(in) :: numele, nnod_4_ele
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: numele_in_core
      integer(kind=kint), intent(in)                                    &
     &                    :: iele_in_core_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iele_in_core(numele_in_core)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: an(num_t_linear,ntot_int_3d)
!
      integer(kind=kint), intent(in) :: numnod, ntot_phys, i_mphi
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
!
      real(kind = kreal), intent(inout) :: ave_mp_core_local
!
      real (kind=kreal) :: bulk_e_smp(np_smp)
!
      integer (kind=kint):: inod1, inod2, ist, ied, k1
      integer (kind = kint) :: iproc, ii, ix, inod, iele, inum
!
!
        ave_mp_core_local = 0.0d0
        bulk_e_smp = 0.0d0
!
!$omp parallel do private(k1,iele,ist,ied,inod1,inod2) 
      do iproc = 1, np_smp
        ist = iele_in_core_smp_stack(iproc-1)+1
        ied = iele_in_core_smp_stack(iproc)
!
        do k1 = 1, num_t_linear
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
            do inum = ist, ied
              iele = iele_in_core(inum)
              inod = ie(iele,k1)
!
              bulk_e_smp(iproc) = bulk_e_smp(iproc)                     &
     &                      + dble(interior_ele(iele))                  &
     &                       * d_nod(inod,i_mphi)                       &
     &                       * an(k1,ix)*xjac(iele,ix)*owe3d(ix)
            end do
          end do
         end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        ave_mp_core_local = ave_mp_core_local + bulk_e_smp(iproc)
      end do
!
      end subroutine fem_icore_mag_potential_icore
!
! ----------------------------------------------------------------------
!
      end module cal_average_mag_potential
