!
!     module cal_fluxes
!
!      Written by H. Matsui
!
!       subroutine cal_flux_vector(i_r, i_s, i_v)
!       subroutine cal_flux_vector_w_reftemp(i_r, i_v, i_s)
!       subroutine cal_flux_tensor(i_r, i_v1, i_v2)
!       subroutine cal_maxwell_tensor(i_r, i_v1)
!       subroutine cal_induction_tensor(i_r, i_v1, i_v2)
!
      module cal_fluxes
!
      use m_precision
!
      use m_geometry_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine cal_flux_vector(i_r, i_v, i_s)
!
      use m_node_phys_data
!
       integer(kind = kint), intent(in) :: i_r, i_s, i_v
       integer(kind = kint) :: inod
!
!$omp parallel do
       do inod = 1, numnod
        d_nod(inod,i_r  ) = d_nod(inod,i_v  ) * d_nod(inod,i_s)
        d_nod(inod,i_r+1) = d_nod(inod,i_v+1) * d_nod(inod,i_s)
        d_nod(inod,i_r+2) = d_nod(inod,i_v+2) * d_nod(inod,i_s)
      end do
!$omp end parallel do
!
       end subroutine cal_flux_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_flux_tensor(i_r, i_v1, i_v2)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      integer(kind = kint) :: inod
!
!$omp parallel do
       do inod = 1, numnod
        d_nod(inod,i_r  ) = d_nod(inod,i_v1  ) * d_nod(inod,i_v2  )
        d_nod(inod,i_r+1) = d_nod(inod,i_v1+1) * d_nod(inod,i_v2  )
        d_nod(inod,i_r+2) = d_nod(inod,i_v1+2) * d_nod(inod,i_v2  )
        d_nod(inod,i_r+3) = d_nod(inod,i_v1+1) * d_nod(inod,i_v2+1)
        d_nod(inod,i_r+4) = d_nod(inod,i_v1+2) * d_nod(inod,i_v2+1)
        d_nod(inod,i_r+5) = d_nod(inod,i_v1+2) * d_nod(inod,i_v2+2)
      end do
!$omp end parallel do
!
       end subroutine cal_flux_tensor
!
!-----------------------------------------------------------------------
!
      subroutine cal_maxwell_tensor(i_r, i_v1)
!
      use m_node_phys_data
      use m_physical_property
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      integer(kind = kint) :: inod
!
!$omp parallel do
       do inod = 1, numnod
         d_nod(inod,i_r  )                                              &
     &                  = ( d_nod(inod,i_v1  )+ex_magne(1) )            &
     &                  * ( d_nod(inod,i_v1  )+ex_magne(1) )
         d_nod(inod,i_r+1)                                              &
     &                  = ( d_nod(inod,i_v1  )+ex_magne(1) )            &
     &                  * ( d_nod(inod,i_v1+1)+ex_magne(2) )
         d_nod(inod,i_r+2)                                              &
     &                  = ( d_nod(inod,i_v1  )+ex_magne(1) )            &
     &                  * ( d_nod(inod,i_v1+2)+ex_magne(3) )
         d_nod(inod,i_r+3)                                              &
     &                  = ( d_nod(inod,i_v1+1)+ex_magne(2) )            &
     &                  * ( d_nod(inod,i_v1+1)+ex_magne(2) )
         d_nod(inod,i_r+4)                                              &
     &                  = ( d_nod(inod,i_v1+1)+ex_magne(2) )            &
     &                  * ( d_nod(inod,i_v1+2)+ex_magne(3) )
         d_nod(inod,i_r+5)                                              &
     &                  = ( d_nod(inod,i_v1+2)+ex_magne(3) )            &
     &                  * ( d_nod(inod,i_v1+2)+ex_magne(3) )
      end do
!$omp end parallel do
!
       end subroutine cal_maxwell_tensor
!
!-----------------------------------------------------------------------
!
      subroutine cal_induction_tensor(i_r, i_v1, i_v2)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      integer(kind = kint) :: inod
!
!$omp parallel do
       do inod = 1, numnod
        d_nod(inod,i_r  ) = d_nod(inod,i_v1+1) * d_nod(inod,i_v2  )     &
     &                     - d_nod(inod,i_v2+1) * d_nod(inod,i_v1  )
        d_nod(inod,i_r+1) = d_nod(inod,i_v1+2) * d_nod(inod,i_v2  )     &
     &                     - d_nod(inod,i_v2+2) * d_nod(inod,i_v1  )
        d_nod(inod,i_r+2) = d_nod(inod,i_v1+2) * d_nod(inod,i_v2+1)     &
     &                     - d_nod(inod,i_v2+2) * d_nod(inod,i_v1+1)
       end do
!$omp end parallel do
!
       end subroutine cal_induction_tensor
!
!-----------------------------------------------------------------------
!
      end module cal_fluxes
