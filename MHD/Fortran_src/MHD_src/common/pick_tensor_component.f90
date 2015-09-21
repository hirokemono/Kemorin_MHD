!
!      module pick_tensor_component
!
!      programmed by H. Matsui on Oct., 2005
!
!!      subroutine pick_sym_tensor_component                            &
!!     &         (numnod, ncomp_nod, i_comp, i_flux, nd, d_nod)
!!      subroutine pick_asym_tensor_component                           &
!!     &         (numnod, ncomp_nod, i_comp, i_flux, nd, d_nod)
!!       i_comp: solution vector ID
!!       i_flux: tensor ID
!!       nd: direction for pick up
!
      module pick_tensor_component
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine pick_sym_tensor_component                              &
     &         (numnod, ncomp_nod, i_comp, i_flux, nd, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_comp, i_flux, nd
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, n1, n2, n3
!
!
      n1 = i_flux + l_sim_t(nd,1)
      n2 = i_flux + l_sim_t(nd,2)
      n3 = i_flux + l_sim_t(nd,3)
!
!$omp parallel do
      do inod = 1, numnod
        d_nod(inod,i_comp  ) = d_nod(inod,n1)
        d_nod(inod,i_comp+1) = d_nod(inod,n2)
        d_nod(inod,i_comp+2) = d_nod(inod,n3)
      end do
!$omp end parallel do
!
!
      end subroutine pick_sym_tensor_component
!
!-----------------------------------------------------------------------
!
      subroutine pick_asym_tensor_component                             &
     &         (numnod, ncomp_nod, i_comp, i_flux, nd, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_comp, i_flux, nd
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, n1, n2, n3
!
!
      n1 = i_flux + l_asim_t(nd,1,1)
      n2 = i_flux + l_asim_t(nd,2,1)
      n3 = i_flux + l_asim_t(nd,3,1)
!
!$omp parallel do
      do inod = 1, numnod
        d_nod(inod,i_comp  ) = dble(l_asim_t(nd,1,2)) * d_nod(inod,n1)
        d_nod(inod,i_comp+1) = dble(l_asim_t(nd,2,2)) * d_nod(inod,n2)
        d_nod(inod,i_comp+2) = dble(l_asim_t(nd,3,2)) * d_nod(inod,n3)
      end do
!$omp end parallel do
!
      end subroutine pick_asym_tensor_component
!
!-----------------------------------------------------------------------
!
      end module pick_tensor_component
