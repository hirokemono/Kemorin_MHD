!cal_similarity_terms
!     module cal_similarity_terms
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_flux_vector                                  &
!!     &         (inod_smp_stack, i_r, i_v, i_s, icm_sgs)
!!      subroutine cal_sgs_flux_tensor                                  &
!!     &         (inod_smp_stack, i_r, i_v1, i_v2, icm_sgs)
!!
!!      subroutine subctract_maxwell_tensor                             &
!!     &         (inod_smp_stack, i_r, i_v1, icm_sgs)
!!      subroutine subctract_uxb_vector                                 &
!!     &         (inod_smp_stack, i_r, i_v1, i_v2)
!!      subroutine subctract_induction_tensor                           &
!!     &         (inod_smp_stack, i_r, i_v1, i_v2, icm_sgs)
!!
!!      subroutine multi_model_coefs_vect_simi                          &
!!     &         (i_r, icm_sgs, inod_smp_stack)
!!      subroutine multi_model_coefs_tensor_simi                        &
!!     &         (inod_smp_stack, i_r, icm_sgs)
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
      subroutine cal_sgs_flux_vector                                    &
     &         (inod_smp_stack, i_r, i_v, i_s, icm_sgs)
!
      use m_node_phys_data
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_s, i_v, icm_sgs
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
      subroutine cal_sgs_flux_tensor                                    &
     &         (inod_smp_stack, i_r, i_v1, i_v2, icm_sgs)
!
      use m_node_phys_data
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2, icm_sgs
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
      subroutine subctract_maxwell_tensor                               &
     &         (inod_smp_stack, i_r, i_v1, icm_sgs)
!
      use m_node_phys_data
      use m_physical_property
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, icm_sgs
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
      subroutine subctract_uxb_vector                                   &
     &         (inod_smp_stack, i_r, i_v1, i_v2)
!
      use m_node_phys_data
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
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
      subroutine subctract_induction_tensor                             &
     &         (inod_smp_stack, i_r, i_v1, i_v2, icm_sgs)
!
      use m_node_phys_data
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2, icm_sgs
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
      subroutine multi_model_coefs_vect_simi                            &
     &         (i_r, icm_sgs, inod_smp_stack)
!
      use m_node_phys_data
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, icm_sgs
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
      subroutine multi_model_coefs_tensor_simi                          &
     &         (inod_smp_stack, i_r, icm_sgs)
!
      use m_node_phys_data
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, icm_sgs
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
