!>@file   t_sph_monitor_data_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief DAta structure for monitor data IO
!!
!!@verbatim
!!      subroutine alloc_layer_spectr_data_IO(ncomp, ltr, nri,          &
!!     &                                      l_spec_IO)
!!      subroutine alloc_layer_mean_data_IO(ncomp, nri, l_mean_IO)
!!      subroutine alloc_volume_spectr_data_IO(ncomp, ltr, v_spec_IO)
!!      subroutine alloc_volume_mean_data_IO(ncomp, v_mean_IO)
!!      subroutine dealloc_layer_spectr_data_IO(l_spec_IO)
!!      subroutine dealloc_layer_mean_data_IO(l_mean_IO)
!!      subroutine dealloc_volume_spectr_data_IO(v_spec_IO)
!!      subroutine dealloc_volume_mean_data_IO(v_mean_IO)
!!        integer(kind = kint), intent(in) :: ncomp, ltr, nri
!!        type(layer_spectr_data_IO), intent(inout) :: l_spec_IO
!!        type(layer_mean_data_IO), intent(inout) :: l_mean_IO
!!        type(volume_spectr_data_IO), intent(inout) :: v_spec_IO
!!        type(volume_mean_data_IO), intent(inout) :: v_mean_IO
!!@endverbatim
      module t_sph_monitor_data_IO
!
      use m_precision
      use m_constants
      use t_sph_spectr_head_labels
!
      implicit none
!
      type layer_spectr_data_IO
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
        real(kind = kreal), allocatable :: spec_r_IO(:,:,:)
      end type layer_spectr_data_IO
!
      type layer_mean_data_IO
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
        real(kind = kreal), allocatable :: sq_r_IO(:,:)
      end type layer_mean_data_IO
!
      type volume_spectr_data_IO
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
        real(kind = kreal), allocatable :: spec_v_IO(:,:)
      end type volume_spectr_data_IO
!
      type volume_mean_data_IO
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
        real(kind = kreal), allocatable :: sq_v_IO(:)
      end type volume_mean_data_IO
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_layer_spectr_data_IO(ncomp, ltr, nri,            &
     &                                      l_spec_IO)
!
      integer(kind = kint), intent(in) :: ncomp, ltr, nri
      type(layer_spectr_data_IO), intent(inout) :: l_spec_IO
!
      allocate(l_spec_IO%spec_r_IO(ncomp,0:ltr,nri))
!
!$omp parallel workshare
      l_spec_IO%spec_r_IO(1:ncomp,0:ltr,1:nri) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_layer_spectr_data_IO
!
!   --------------------------------------------------------------------
!
      subroutine alloc_layer_mean_data_IO(ncomp, nri, l_mean_IO)
!
      integer(kind = kint), intent(in) :: ncomp, nri
      type(layer_mean_data_IO), intent(inout) :: l_mean_IO
!
      allocate(l_mean_IO%sq_r_IO(ncomp,nri))
!
!$omp parallel workshare
      l_mean_IO%sq_r_IO(1:ncomp,1:nri) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_layer_mean_data_IO
!
!   --------------------------------------------------------------------
!
      subroutine alloc_volume_spectr_data_IO(ncomp, ltr, v_spec_IO)
!
      integer(kind = kint), intent(in) :: ncomp, ltr
      type(volume_spectr_data_IO), intent(inout) :: v_spec_IO
!
      allocate(v_spec_IO%spec_v_IO(ncomp,0:ltr))
!
!$omp parallel workshare
      v_spec_IO%spec_v_IO(1:ncomp,0:ltr) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_volume_spectr_data_IO
!
!   --------------------------------------------------------------------
!
      subroutine alloc_volume_mean_data_IO(ncomp, v_mean_IO)
!
      integer(kind = kint), intent(in) :: ncomp
      type(volume_mean_data_IO), intent(inout) :: v_mean_IO
!
      allocate(v_mean_IO%sq_v_IO(ncomp))
!
!$omp parallel workshare
      v_mean_IO%sq_v_IO(1:ncomp) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_volume_mean_data_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_layer_spectr_data_IO(l_spec_IO)
!
      type(layer_spectr_data_IO), intent(inout) :: l_spec_IO
!
      deallocate(l_spec_IO%spec_r_IO)
!
      end subroutine dealloc_layer_spectr_data_IO
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_layer_mean_data_IO(l_mean_IO)
!
      type(layer_mean_data_IO), intent(inout) :: l_mean_IO
!
      deallocate(l_mean_IO%sq_r_IO)
!
      end subroutine dealloc_layer_mean_data_IO
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_volume_spectr_data_IO(v_spec_IO)
!
      type(volume_spectr_data_IO), intent(inout) :: v_spec_IO
!
      deallocate(v_spec_IO%spec_v_IO)
!
      end subroutine dealloc_volume_spectr_data_IO
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_volume_mean_data_IO(v_mean_IO)
!
      type(volume_mean_data_IO), intent(inout) :: v_mean_IO
!
      deallocate(v_mean_IO%sq_v_IO)
!
      end subroutine dealloc_volume_mean_data_IO
!
!   --------------------------------------------------------------------
!
      end module t_sph_monitor_data_IO
