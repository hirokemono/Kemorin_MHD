!sgs_terms_2_each_ele.f90
!      module sgs_terms_2_each_ele
!
!      Written by H. Matsui on July, 2005
!
!      subroutine SGS_v_flux_2_each_element(k2, i_vect, i_scalar,       &
!     &          i_flux, flux_e)
!      subroutine SGS_m_flux_2_each_element(k2, i_v1, i_flux,           &
!     &          flux_e)
!      subroutine SGS_induct_2_each_element(k2, i_b, i_v,               &
!     &          i_flux, flux_a)
!
      module sgs_terms_2_each_ele
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_v_flux_2_each_element(k2, i_vect, i_scalar,        &
     &          i_flux, flux_e)
!
      use m_node_phys_data
      use m_phys_constants
!
       real (kind=kreal), intent(inout) :: flux_e(numele,3)
       integer(kind = kint), intent(in) :: k2
       integer(kind = kint), intent(in) :: i_vect, i_scalar, i_flux
       integer(kind = kint) :: iproc, inod, iele
       integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied) 
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           flux_e(iele,1) = d_nod(inod,i_vect  )*d_nod(inod,i_scalar)   &
     &                     + d_nod(inod,i_flux  )
           flux_e(iele,2) = d_nod(inod,i_vect+1)*d_nod(inod,i_scalar)   &
     &                     + d_nod(inod,i_flux+1)
           flux_e(iele,3) = d_nod(inod,i_vect+2)*d_nod(inod,i_scalar)   &
     &                     + d_nod(inod,i_flux+2)
         end do
       end do
!$omp end parallel do
!
      end subroutine SGS_v_flux_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_m_flux_2_each_element(k2, i_v1, i_flux, flux_e)
!
      use m_node_phys_data
      use m_phys_constants
!
       real (kind=kreal), intent(inout) :: flux_e(numele,6)
       integer(kind = kint), intent(in) :: k2
       integer(kind = kint), intent(in) :: i_v1, i_flux
       integer(kind = kint) :: iproc, inod, iele
       integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied) 
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           flux_e(iele,1) = d_nod(inod,i_v1  )*d_nod(inod,i_v1  )       &
     &                     + d_nod(inod,i_flux  )
           flux_e(iele,2) = d_nod(inod,i_v1+1)*d_nod(inod,i_v1  )       &
     &                     + d_nod(inod,i_flux+1)
           flux_e(iele,3) = d_nod(inod,i_v1+2)*d_nod(inod,i_v1  )       &
     &                     + d_nod(inod,i_flux+2)
           flux_e(iele,4) = d_nod(inod,i_v1+1)*d_nod(inod,i_v1+1)       &
     &                     + d_nod(inod,i_flux+3)
           flux_e(iele,5) = d_nod(inod,i_v1+2)*d_nod(inod,i_v1+1)       &
     &                     + d_nod(inod,i_flux+4)
           flux_e(iele,6) = d_nod(inod,i_v1+2)*d_nod(inod,i_v1+2)       &
     &                     + d_nod(inod,i_flux+5)
         end do
       end do
!$omp end parallel do
!
      end subroutine SGS_m_flux_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_2_each_element(k2, i_b, i_v,                &
     &          i_flux, flux_a)
!
      use m_node_phys_data
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_flux
!
      real (kind=kreal), intent(inout) :: flux_a(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied)
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           flux_a(iele,1) =  d_nod(inod,i_flux  )                       &
     &                     + d_nod(inod,i_b+1)*d_nod(inod,i_v  )        &
     &                     - d_nod(inod,i_v+1)*d_nod(inod,i_b  )
           flux_a(iele,2) =   d_nod(inod,i_flux+1)                      &
     &                     + d_nod(inod,i_b+2)*d_nod(inod,i_v+1)        &
     &                     - d_nod(inod,i_v+2)*d_nod(inod,i_b+1)
           flux_a(iele,3) =  d_nod(inod,i_flux+2)                       &
     &                     + d_nod(inod,i_b  )*d_nod(inod,i_v+2)        &
     &                     - d_nod(inod,i_v  )*d_nod(inod,i_b+2)
         end do
       end do
!$omp end parallel do
!
!
      end subroutine SGS_induct_2_each_element
!
!  ---------------------------------------------------------------------
!
      end module sgs_terms_2_each_ele
