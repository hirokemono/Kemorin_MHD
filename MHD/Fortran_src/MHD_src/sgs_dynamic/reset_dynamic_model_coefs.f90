!
!      module reset_dynamic_model_coefs
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!
!      subroutine reset_vector_sgs_model_coefs(icomp_sgs)
!      subroutine reset_tensor_sgs_model_coefs(icomp_sgs)
!
!      subroutine reset_diff_model_coefs(iak_diff)
!
!      subroutine reset_vector_sgs_nod_m_coefs(icomp_sgs)
!      subroutine reset_tensor_sgs_nod_m_coefs(icomp_sgs)
!
      module reset_dynamic_model_coefs
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_machine_parameter
      use m_SGS_model_coefs
      use m_layering_ele_list
      use m_ele_info_4_dynamical
!
      implicit none
!
      private :: reset_sgs_v_model_coefs_elesmp
      private :: reset_sgs_t_model_coefs_elesmp
      private :: reset_sgs_v_model_coefs_grpsmp
      private :: reset_sgs_t_model_coefs_grpsmp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine reset_vector_sgs_model_coefs(icomp_sgs)
!
      integer (kind = kint), intent(in) :: icomp_sgs
!
!
      if (minlayer_4_smp .gt. min_item_layer_d_smp) then
        call reset_sgs_v_model_coefs_elesmp(icomp_sgs)
      else
        call reset_sgs_v_model_coefs_grpsmp(icomp_sgs)
      end if
!
      end subroutine reset_vector_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine reset_tensor_sgs_model_coefs(icomp_sgs)
!
      integer (kind = kint), intent(in) :: icomp_sgs
!
!
      if (minlayer_4_smp .gt. min_item_layer_d_smp) then
        call reset_sgs_t_model_coefs_elesmp(icomp_sgs)
      else
        call reset_sgs_t_model_coefs_grpsmp(icomp_sgs)
      end if
!
      end subroutine reset_tensor_sgs_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_v_model_coefs_elesmp(icomp_sgs)
!
      integer (kind = kint), intent(in) :: icomp_sgs
      integer (kind = kint) :: iele0, iele, iproc, ist, ied, is, inum
!
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_sgs(iele, icomp_sgs  ) = zero
          ak_sgs(iele, icomp_sgs+1) = zero
          ak_sgs(iele, icomp_sgs+2) = zero
        end do
      end do
!$omp end parallel do
!
!$omp parallel
      do inum = 1, n_layer_d
!$omp do private(iele0,iele,is,ist,ied) 
        do iproc = 1, np_smp
          is = (inum-1)*np_smp + iproc
          ist = layer_stack_smp(is-1) + 1
          ied = layer_stack_smp(is  )
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine reset_sgs_v_model_coefs_elesmp
!
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_t_model_coefs_elesmp(icomp_sgs)
!
      integer (kind = kint), intent(in) :: icomp_sgs
      integer (kind = kint) :: iele0, iele, iproc, ist, ied, is, inum
!
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_sgs(iele, icomp_sgs  ) = zero
          ak_sgs(iele, icomp_sgs+1) = zero
          ak_sgs(iele, icomp_sgs+2) = zero
          ak_sgs(iele, icomp_sgs+3) = zero
          ak_sgs(iele, icomp_sgs+4) = zero
          ak_sgs(iele, icomp_sgs+5) = zero
        end do
      end do
!$omp end parallel do
!
!$omp parallel
      do inum = 1, n_layer_d
!$omp do private(iele0,iele,is,ist,ied) 
        do iproc = 1, np_smp
          is = (inum-1)*np_smp + iproc
          ist = layer_stack_smp(is-1) + 1
          ied = layer_stack_smp(is  )
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
            ak_sgs(iele, icomp_sgs+3) = one
            ak_sgs(iele, icomp_sgs+4) = one
            ak_sgs(iele, icomp_sgs+5) = one
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine reset_sgs_t_model_coefs_elesmp
!
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_v_model_coefs_grpsmp(icomp_sgs)
!
      use m_layering_ele_list
!
      integer (kind = kint), intent(in) :: icomp_sgs
      integer (kind = kint) :: iele0, iele, iproc, inum
      integer (kind = kint) :: ist, ied, ist_num, ied_num
!
!
!$omp parallel do private(iele0,iele,ist,ied,ist_num,ied_num,inum)
      do iproc = 1, np_smp
        ist_num = istack_item_layer_d_smp(iproc-1) + 1
        ied_num = istack_item_layer_d_smp(iproc  )
        do inum = ist_num, ied_num
          ist = layer_stack(inum-1) + 1
          ied = layer_stack(inum)
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_sgs_v_model_coefs_grpsmp
!
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_t_model_coefs_grpsmp(icomp_sgs)
!
      use m_layering_ele_list
!
      integer (kind = kint), intent(in) :: icomp_sgs
      integer (kind = kint) :: iele0, iele, iproc, inum
      integer (kind = kint) :: ist, ied, ist_num, ied_num
!
!
!$omp parallel do private(iele0,iele,ist,ied,ist_num,ied_num,inum)
      do iproc = 1, np_smp
        ist_num = istack_item_layer_d_smp(iproc-1) + 1
        ied_num = istack_item_layer_d_smp(iproc  )
        do inum = ist_num, ied_num
          ist = layer_stack(inum-1) + 1
          ied = layer_stack(inum)
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
            ak_sgs(iele, icomp_sgs+3) = one
            ak_sgs(iele, icomp_sgs+4) = one
            ak_sgs(iele, icomp_sgs+5) = one
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_sgs_t_model_coefs_grpsmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_diff_model_coefs(iak_diff)
!
      integer (kind = kint), intent(in) :: iak_diff
      integer (kind = kint) :: iele, iproc, ist, ied
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_diff(iele, iak_diff) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_diff_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_vector_sgs_nod_m_coefs(icomp_sgs)
!
      integer (kind = kint), intent(in) :: icomp_sgs
      integer (kind = kint) :: inod, iproc, ist, ied
!
!$omp parallel do private(inod, ist, ied) 
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied = inod_smp_stack(iproc)
        do inod = ist, ied
          ak_sgs_nod(inod, icomp_sgs  ) = one
          ak_sgs_nod(inod, icomp_sgs+1) = one
          ak_sgs_nod(inod, icomp_sgs+2) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_vector_sgs_nod_m_coefs
!
!-----------------------------------------------------------------------
!
      subroutine reset_tensor_sgs_nod_m_coefs(icomp_sgs)
!
      integer (kind = kint), intent(in) :: icomp_sgs
      integer (kind = kint) :: inod, iproc, ist, ied
!
!$omp parallel do private(inod, ist, ied) 
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied = inod_smp_stack(iproc)
        do inod = ist, ied
          ak_sgs_nod(inod, icomp_sgs  ) = one
          ak_sgs_nod(inod, icomp_sgs+1) = one
          ak_sgs_nod(inod, icomp_sgs+2) = one
          ak_sgs_nod(inod, icomp_sgs+3) = one
          ak_sgs_nod(inod, icomp_sgs+4) = one
          ak_sgs_nod(inod, icomp_sgs+5) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_tensor_sgs_nod_m_coefs
!
!-----------------------------------------------------------------------
!
      end module reset_dynamic_model_coefs
