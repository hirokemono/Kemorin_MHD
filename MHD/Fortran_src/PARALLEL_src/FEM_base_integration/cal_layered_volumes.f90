!
!      module cal_layered_volumes
!
!     Written by H. Matsui on Aug., 2007
!
!  Volume integration: s_cal_layered_volumes
!      subroutine s_cal_layered_volumes
!      subroutine deallocate_layering_volumes
!
      module cal_layered_volumes
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
!
      implicit none
!
      real(kind = kreal), allocatable :: vol_l(:)
      real(kind = kreal), allocatable :: vol_l_smp(:)
      real(kind = kreal) :: vol_w
      private :: vol_l_smp, vol_l, vol_w
!
      private :: int_volume_4_sgs_layer, int_volume_dynamic_grpsmp
      private :: allocate_work_layerd_volume
      private :: deallocate_work_layerd_volume
      private :: sum_volumes_4_layerd, cal_a_vol_4_layerd
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_layered_volumes
!
      use m_layering_ele_list
!
      integer (kind = kint) :: inum
!
!
      call alloc_layering_volumes_type(layer_tbl1)
      call allocate_work_layerd_volume(layer_tbl1%n_layer_d)
!
!      if(layer_tbl1%minlayer_4_smp                                     &
!     &      .gt. layer_tbl1%min_item_layer_d_smp) then
        if (iflag_debug.eq.1) write(*,*) 'int_volume_4_sgs_layer'
        call int_volume_4_sgs_layer                                     &
     &     (layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack_smp, layer_tbl1%item_layer)
!      else
!        if (iflag_debug.eq.1) write(*,*) 'int_volume_dynamic_grpsmp'
!        call int_volume_dynamic_grpsmp                                 &
!     &     (layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,           &
!     &      layer_tbl1%layer_stack, layer_tbl1%istack_item_layer_d_smp,&
!     &      layer_tbl1%item_layer)
!      end if
!
      if (iflag_debug.eq.1) write(*,*) 'sum_volumes_4_layerd'
      call sum_volumes_4_layerd(layer_tbl1%n_layer_d,                   &
     &   layer_tbl1%volumes_layer, layer_tbl1%vol_total_layer)
      if (iflag_debug.eq.1) write(*,*) 'cal_a_vol_4_layerd'
      call cal_a_vol_4_layerd                                           &
     &   (layer_tbl1%n_layer_d, layer_tbl1%volumes_layer,               &
     &    layer_tbl1%vol_total_layer, layer_tbl1%a_vol_layer,           &
     &    layer_tbl1%a_vol_total_layer)
!
      call deallocate_work_layerd_volume
!
      if (iflag_debug.eq.1) then
        write(*,*) 'vol_total_layer: ', layer_tbl1%vol_total_layer(1)
        write(*,*) 'layer, volumes_layer'
        do inum = 1, layer_tbl1%n_layer_d
          write(*,*) inum, layer_tbl1%volumes_layer(inum)
        end do
      end if
!
      end subroutine s_cal_layered_volumes
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_layering_volumes
!
      use m_layering_ele_list
!
      call dealloc_layering_volumes_type(layer_tbl1)
!
      end subroutine deallocate_layering_volumes
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine  allocate_work_layerd_volume(n_layer_d)
!
      integer (kind = kint), intent(in) :: n_layer_d
!
!
      allocate (vol_l(n_layer_d))
      allocate (vol_l_smp(np_smp))
!
      if(n_layer_d .gt. 0) vol_l = zero
      vol_l_smp = zero
!
      end subroutine  allocate_work_layerd_volume
!
!  ---------------------------------------------------------------------
!
      subroutine  deallocate_work_layerd_volume
!
!
      deallocate (vol_l, vol_l_smp)
!
      end subroutine  deallocate_work_layerd_volume
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_volume_4_sgs_layer(n_layer_d, n_item_layer_d,      &
     &          layer_stack_smp, item_layer)
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      integer (kind = kint) :: iproc, iele, iele0
      integer (kind = kint) :: is, ist, ied, igrp
!
!
      vol_w = zero
      vol_l(1:n_layer_d) = zero
!
      do igrp = 1, n_layer_d
!
        vol_l_smp(1:np_smp) = zero
!$omp parallel do private(is,ist,ied,iele0,iele)
        do iproc = 1, np_smp
          is = (igrp-1)*np_smp + iproc
          ist = layer_stack_smp(is-1) + 1
          ied = layer_stack_smp(is  )
!
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            vol_l_smp(iproc) = vol_l_smp(iproc)                         &
     &                        + volume_ele(iele) * e_multi(iele)
          end do
        end do
!$omp end parallel do
!
        do iproc = 1, np_smp
          vol_l(igrp) = vol_l(igrp) + vol_l_smp(iproc)
        end do
        vol_w = vol_w + vol_l(igrp)
      end do
!
      end subroutine int_volume_4_sgs_layer
!
!  ---------------------------------------------------------------------
!
      subroutine int_volume_dynamic_grpsmp(n_layer_d, n_item_layer_d,   &
     &           layer_stack, istack_item_layer_d_smp, item_layer)
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in)                                 &
     &               :: istack_item_layer_d_smp(0:np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      integer (kind = kint) :: igrp, iele, iele0, iproc
      integer (kind = kint) :: ist, ied, ist_num, ied_num
!
!
      vol_l = zero
      vol_w = zero
      vol_l_smp = zero
!
!$omp parallel do &
!$omp& private(ist_num,ied_num,igrp,ist,ied,iele,iele0)
      do iproc = 1, np_smp
        ist_num = istack_item_layer_d_smp(iproc-1) + 1
        ied_num = istack_item_layer_d_smp(iproc  )
!
        vol_l(ist_num:ied_num) = zero
        do igrp = ist_num, ied_num
          ist = layer_stack(igrp-1) + 1
          ied = layer_stack(igrp)
!
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            vol_l(igrp) = vol_l(igrp)                                   &
     &                   + volume_ele(iele) * e_multi(iele)
          end do
          vol_l_smp(iproc) = vol_l_smp(iproc) + vol_l(igrp)
        end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        vol_w = vol_w + vol_l_smp(iproc)
      end do
!
      end subroutine int_volume_dynamic_grpsmp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sum_volumes_4_layerd                                   &
     &          (n_layer_d, volumes_layer, vol_total_layer)
!
      use calypso_mpi
!
      integer (kind = kint), intent(in) :: n_layer_d
      real(kind = kreal), intent(inout) :: volumes_layer(n_layer_d)
      real(kind = kreal), intent(inout) :: vol_total_layer(1)
!
!
      volumes_layer =   zero
      vol_total_layer = zero
      call MPI_allREDUCE ( vol_l, volumes_layer, n_layer_d,             &
     &     CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE ( vol_w, vol_total_layer, ione,                &
     &     CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_volumes_4_layerd
!
!  ---------------------------------------------------------------------
!
      subroutine cal_a_vol_4_layerd(n_layer_d, volumes_layer,           &
     &         vol_total_layer, a_vol_layer, a_vol_total_layer)
!
      integer (kind = kint), intent(in) :: n_layer_d
      real(kind = kreal), intent(in) :: volumes_layer(n_layer_d)
      real(kind = kreal), intent(in) :: vol_total_layer(1)
      real(kind = kreal), intent(inout) :: a_vol_layer(n_layer_d)
      real(kind = kreal), intent(inout) :: a_vol_total_layer(1)
!
      integer (kind = kint) :: inum
!
!
      do inum = 1, n_layer_d
        if (volumes_layer(inum) .eq. zero) then
          a_vol_layer(inum) = 1.0d30
        else
          a_vol_layer(inum) = one / volumes_layer(inum)
        end if
      end do
!
      if (vol_total_layer(1) .eq. zero) then
        a_vol_total_layer(1) = 1.0d30
      else
        a_vol_total_layer(1) = one / vol_total_layer(1)
      end if
!
      end subroutine cal_a_vol_4_layerd
!
!  ---------------------------------------------------------------------
!
      end module cal_layered_volumes
