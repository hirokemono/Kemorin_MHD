!cal_similarity_terms
!     module cal_similarity_terms
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_flux_vector(numnod, inod_smp_stack,          &
!!     &          ncomp_nod, i_r, i_v, i_s, ntot_comp_nod, icm_sgs,     &
!!     &          ak_sgs_nod, d_nod)
!!      subroutine cal_sgs_flux_tensor(numnod, inod_smp_stack,          &
!!     &          ncomp_nod, i_r, i_v1, i_v2, ntot_comp_nod, icm_sgs,   &
!!     &          ak_sgs_nod, d_nod)
!!
!!      subroutine subctract_maxwell_tensor(numnod, inod_smp_stack,     &
!!     &          ncomp_nod, i_r, i_v1, ntot_comp_nod, icm_sgs,         &
!!     &          ak_sgs_nod, d_nod)
!!      subroutine subctract_uxb_vector(numnod, inod_smp_stack,         &
!!     &         ncomp_nod, i_r, i_v1, i_v2, d_nod)
!!      subroutine subctract_induction_tensor(numnod, inod_smp_stack,   &
!!     &          ncomp_nod, i_r, i_v1, i_v2, ntot_comp_nod, icm_sgs,   &
!!     &          ak_sgs_nod, d_nod)
!!
!!      subroutine multi_model_coefs_vect_simi(numnod, inod_smp_stack,  &
!!     &          ncomp_nod, i_r, ntot_comp_nod, icm_sgs, ak_sgs_nod,   &
!!     &          d_nod)
!!      subroutine multi_model_coefs_tensor_simi(numnod, inod_smp_stack,&
!!     &         ncomp_nod, i_r, ntot_comp_nod, icm_sgs, ak_sgs_nod,    &
!!     &         d_nod)
!
      module cal_similarity_terms
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_flux_vector(numnod, inod_smp_stack,            &
     &          ncomp_nod, i_r, i_v, i_s, ntot_comp_nod, icm_sgs,       &
     &          ak_sgs_nod, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_s, i_v
      integer(kind = kint), intent(in) :: ntot_comp_nod, icm_sgs
      real(kind = kreal), intent(in)                                    &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_r  ) = ak_sgs_nod(inod,icm_sgs)                  &
     &                       * ( d_nod(inod,i_r  )                      &
     &                         - d_nod(inod,i_v  ) * d_nod(inod,i_s) )
          d_nod(inod,i_r+1) = ak_sgs_nod(inod,icm_sgs+1)                &
     &                       * ( d_nod(inod,i_r+1)                      &
     &                         - d_nod(inod,i_v+1) * d_nod(inod,i_s) )
          d_nod(inod,i_r+2) = ak_sgs_nod(inod,icm_sgs+2)                &
     &                       * ( d_nod(inod,i_r+2)                      &
     &                         - d_nod(inod,i_v+2) * d_nod(inod,i_s) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sgs_flux_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_flux_tensor(numnod, inod_smp_stack,            &
     &          ncomp_nod, i_r, i_v1, i_v2, ntot_comp_nod, icm_sgs,     &
     &          ak_sgs_nod, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      integer(kind = kint), intent(in) :: ntot_comp_nod, icm_sgs
      real(kind = kreal), intent(in)                                    &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_r  ) = ak_sgs_nod(inod,icm_sgs  )                &
     &                    * ( d_nod(inod,i_r  )                         &
     &                      - d_nod(inod,i_v1  ) * d_nod(inod,i_v2  ) )
          d_nod(inod,i_r+1) = ak_sgs_nod(inod,icm_sgs+1)                &
     &                    * ( d_nod(inod,i_r+1)                         &
     &                      - d_nod(inod,i_v1+1) * d_nod(inod,i_v2  ) )
          d_nod(inod,i_r+2) = ak_sgs_nod(inod,icm_sgs+2)                &
     &                    * ( d_nod(inod,i_r+2)                         &
     &                      - d_nod(inod,i_v1+2) * d_nod(inod,i_v2  ) )
          d_nod(inod,i_r+3) = ak_sgs_nod(inod,icm_sgs+3)                &
     &                    * ( d_nod(inod,i_r+3)                         &
     &                      - d_nod(inod,i_v1+1) * d_nod(inod,i_v2+1) )
          d_nod(inod,i_r+4) = ak_sgs_nod(inod,icm_sgs+4)                &
     &                    * ( d_nod(inod,i_r+4)                         &
     &                      - d_nod(inod,i_v1+2) * d_nod(inod,i_v2+1) )
          d_nod(inod,i_r+5) = ak_sgs_nod(inod,icm_sgs+5)                &
     &                    * ( d_nod(inod,i_r+5)                         &
     &                      - d_nod(inod,i_v1+2) * d_nod(inod,i_v2+2) )
        end do
      end do
!$omp end parallel do
!
       end subroutine cal_sgs_flux_tensor
!
!-----------------------------------------------------------------------
!
      subroutine subctract_maxwell_tensor(numnod, inod_smp_stack,       &
     &          ncomp_nod, i_r, i_v1, ntot_comp_nod, icm_sgs,           &
     &          ak_sgs_nod, d_nod)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1
      integer(kind = kint), intent(in) :: ntot_comp_nod, icm_sgs
      real(kind = kreal), intent(in)                                    &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_r  ) = ak_sgs_nod(inod,icm_sgs  )                &
     &                       * ( d_nod(inod,i_r  )                      &
     &                       - ( d_nod(inod,i_v1  )+ex_magne(1) )       &
     &                       * ( d_nod(inod,i_v1  )+ex_magne(1) ) )
          d_nod(inod,i_r+1) = ak_sgs_nod(inod,icm_sgs+1)                &
     &                       * ( d_nod(inod,i_r+1)                      &
     &                       - ( d_nod(inod,i_v1  )+ex_magne(1) )       &
     &                       * ( d_nod(inod,i_v1+1)+ex_magne(2) ) )
          d_nod(inod,i_r+2) = ak_sgs_nod(inod,icm_sgs+2)                &
     &                       * ( d_nod(inod,i_r+2)                      &
     &                       - ( d_nod(inod,i_v1  )+ex_magne(1) )       &
     &                       * ( d_nod(inod,i_v1+2)+ex_magne(3) ) )
          d_nod(inod,i_r+3) = ak_sgs_nod(inod,icm_sgs+3)                &
     &                       * ( d_nod(inod,i_r+3)                      &
     &                       - ( d_nod(inod,i_v1+1)+ex_magne(2) )       &
     &                       * ( d_nod(inod,i_v1+1)+ex_magne(2) ) )
          d_nod(inod,i_r+4) = ak_sgs_nod(inod,icm_sgs+4)                &
     &                       * ( d_nod(inod,i_r+4)                      &
     &                       - ( d_nod(inod,i_v1+1)+ex_magne(2) )       &
     &                       * ( d_nod(inod,i_v1+2)+ex_magne(3) ) )
          d_nod(inod,i_r+5) = ak_sgs_nod(inod,icm_sgs+5)                &
     &                       * ( d_nod(inod,i_r+5)                      &
     &                       - ( d_nod(inod,i_v1+2)+ex_magne(3) )       &
     &                       * ( d_nod(inod,i_v1+2)+ex_magne(3) ) )
        end do
      end do
!$omp end parallel do
!
      end subroutine subctract_maxwell_tensor
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine subctract_uxb_vector(numnod, inod_smp_stack,           &
     &         ncomp_nod, i_r, i_v1, i_v2, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_r  ) = d_nod(inod,i_r  )                         &
     &                    - ( d_nod(inod,i_v1+1)*d_nod(inod,i_v2+2)     &
     &                      - d_nod(inod,i_v1+2)*d_nod(inod,i_v2+1) )
          d_nod(inod,i_r+1) = d_nod(inod,i_r+1)                         &
     &                    - ( d_nod(inod,i_v1+2)*d_nod(inod,i_v2  )     &
     &                      - d_nod(inod,i_v1  )*d_nod(inod,i_v2+2) )
          d_nod(inod,i_r+2) = d_nod(inod,i_r+2)                         &
     &                    - ( d_nod(inod,i_v1  )*d_nod(inod,i_v2+1)     &
     &                      - d_nod(inod,i_v1+1)*d_nod(inod,i_v2  ) )
        end do
      end do
!$omp end parallel do
!
       end subroutine subctract_uxb_vector
!
!-----------------------------------------------------------------------
!
      subroutine subctract_induction_tensor(numnod, inod_smp_stack,     &
     &          ncomp_nod, i_r, i_v1, i_v2, ntot_comp_nod, icm_sgs,     &
     &          ak_sgs_nod, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ntot_comp_nod, icm_sgs
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      real(kind = kreal), intent(in)                                    &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ip, ist, ied
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_r  ) = ak_sgs_nod(inod,icm_sgs  )                &
     &                     * (d_nod(inod,i_r  )                         &
     &                      - d_nod(inod,i_v1+1) * d_nod(inod,i_v2  )   &
     &                      + d_nod(inod,i_v2+1) * d_nod(inod,i_v1  ) )
          d_nod(inod,i_r+1) = ak_sgs_nod(inod,icm_sgs+1)                &
     &                     * (d_nod(inod,i_r+1)                         &
     &                      - d_nod(inod,i_v1+2) * d_nod(inod,i_v2  )   &
     &                      + d_nod(inod,i_v2+2) * d_nod(inod,i_v1  ) )
          d_nod(inod,i_r+2) = ak_sgs_nod(inod,icm_sgs+2)                &
     &                     * (d_nod(inod,i_r+2)                         &
     &                      - d_nod(inod,i_v1+2) * d_nod(inod,i_v2+1)   &
     &                      + d_nod(inod,i_v2+2) * d_nod(inod,i_v1+1) )
        end do
      end do
!$omp end parallel do
!
       end subroutine subctract_induction_tensor
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine multi_model_coefs_vect_simi(numnod, inod_smp_stack,    &
     &          ncomp_nod, i_r, ntot_comp_nod, icm_sgs, ak_sgs_nod,     &
     &          d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r
      integer(kind = kint), intent(in) :: ntot_comp_nod, icm_sgs
      real(kind = kreal), intent(in)                                    &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_r  ) = ak_sgs_nod(inod,icm_sgs  )                &
     &                       * d_nod(inod,i_r  )
          d_nod(inod,i_r+1) = ak_sgs_nod(inod,icm_sgs+1)                &
     &                       * d_nod(inod,i_r+1)
          d_nod(inod,i_r+2) = ak_sgs_nod(inod,icm_sgs+2)                &
     &                       * d_nod(inod,i_r+2)
        end do
      end do
!$omp end parallel do
!
       end subroutine multi_model_coefs_vect_simi
!
!-----------------------------------------------------------------------
!
      subroutine multi_model_coefs_tensor_simi(numnod, inod_smp_stack,  &
     &         ncomp_nod, i_r, ntot_comp_nod, icm_sgs, ak_sgs_nod,      &
     &         d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r
      integer(kind = kint), intent(in) :: ntot_comp_nod, icm_sgs
      real(kind = kreal), intent(in)                                    &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_r  ) = ak_sgs_nod(inod,icm_sgs  )                &
     &                       * d_nod(inod,i_r  )
          d_nod(inod,i_r+1) = ak_sgs_nod(inod,icm_sgs+1)                &
     &                       * d_nod(inod,i_r+1)
          d_nod(inod,i_r+2) = ak_sgs_nod(inod,icm_sgs+2)                &
     &                       * d_nod(inod,i_r+2)
          d_nod(inod,i_r+3) = ak_sgs_nod(inod,icm_sgs+3)                &
     &                       * d_nod(inod,i_r+3)
          d_nod(inod,i_r+4) = ak_sgs_nod(inod,icm_sgs+4)                &
     &                       * d_nod(inod,i_r+4)
          d_nod(inod,i_r+5) = ak_sgs_nod(inod,icm_sgs+5)                &
     &                       * d_nod(inod,i_r+5)
        end do
      end do
!$omp end parallel do
!
       end subroutine multi_model_coefs_tensor_simi
!
!-----------------------------------------------------------------------
!
      end module cal_similarity_terms
