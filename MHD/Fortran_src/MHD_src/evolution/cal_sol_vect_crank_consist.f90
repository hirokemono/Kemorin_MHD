!
!      module cal_sol_vect_crank_consist
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_pre_consist
!      subroutine cal_sol_temp_consist
!      subroutine cal_sol_vect_p_pre_consist
!      subroutine cal_sol_d_scalar_consist
!
      module cal_sol_vect_crank_consist
!
      use m_precision
!
      implicit none
!
      private :: cal_sol_vec_pre_consist
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_consist
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_velo,           &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_pre_mom, d_nod)
!
      end subroutine cal_sol_velo_pre_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_consist
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_temp,           &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_pre_heat, d_nod)
!
      end subroutine cal_sol_temp_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_consist
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_magne,          &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_pre_uxb, d_nod)
!
      end subroutine cal_sol_vect_p_pre_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_consist
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_light,          &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_pre_composit, d_nod)
!
      end subroutine cal_sol_d_scalar_consist
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vec_pre_consist(numnod, inter_smp_stack,       &
     &          coef_field, ncomp_nod, numdir, if_pre, d_nod)
!
      use m_machine_parameter
      use m_finite_element_matrix
      use m_t_int_parameter
!
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef_field
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: numdir, if_pre
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, nd
      integer (kind = kint) :: if_comp
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,inod,if_comp)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, numdir
          if_comp = if_pre + nd - 1
!cdir nodep
          do inod = ist, ied
!
            ff(inod,nd) = ( ff(inod,nd)                                 &
     &                   + adam_0 * ff_nl(inod,nd)                      &
     &                   + adam_1 * d_nod(inod,if_comp) ) * dt
            d_nod(inod,if_comp) = ff_nl(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      if (coef_field.gt.0.0d0) then
        call cal_ff_smp_2_ff(numdir, ff_m_smp, ff)
      end if
!
      end subroutine cal_sol_vec_pre_consist
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vect_crank_consist
