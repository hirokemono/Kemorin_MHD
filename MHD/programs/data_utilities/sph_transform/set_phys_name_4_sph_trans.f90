!>@file   set_phys_name_4_sph_trans.f90
!!@brief  module set_phys_name_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!
!!@verbatim
!!      subroutine copy_sph_trans_nums_from_rtp(ncomp_sph_trans)
!!      subroutine copy_sph_name_rj_to_rtp(rj_fld)
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!
      module set_phys_name_4_sph_trans
!
      use m_precision
!
      implicit  none
!
!>      Number of fields on spherical grid @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_phys_rtp
!>      Field name for @f$ f(r,\theta,\phi) @f$
      character (len=kchara), allocatable :: phys_name_rtp(:)
!
!>      Number of fields of scalar fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_scalar_rtp
!>      Number of fields of vector fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_vector_rtp
!>      Number of fields of tensor fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_tensor_rtp
!
!>      Start field address of scalar fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: istart_scalar_rtp
!>      Start field address of vector fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: istart_vector_rtp
!>      Start field address of tensor fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: istart_tensor_rtp
!
      private :: allocate_phys_rtp_name
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_phys_rtp_name
!
      allocate( phys_name_rtp(num_phys_rtp) )
!
      end subroutine allocate_phys_rtp_name
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_phys_rtp_name
!
      deallocate( phys_name_rtp )
!
      end subroutine deallocate_phys_rtp_name
!
!  --------------------------------------------------------------------
!
      subroutine copy_sph_trans_nums_from_rtp(ncomp_sph_trans)
!
      use m_machine_parameter
      use m_phys_constants
!
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
!
!
      ncomp_sph_trans =  num_tensor_rtp * n_sym_tensor                  &
     &                 + num_vector_rtp * n_vector                      &
     &                 + num_scalar_rtp * n_scalar
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans', ncomp_sph_trans
      end if
!
      end subroutine copy_sph_trans_nums_from_rtp
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine copy_sph_name_rj_to_rtp(rj_fld)
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_phys_data
!
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: i, i0
!
      num_phys_rtp =  rj_fld%num_phys
      call allocate_phys_rtp_name
!
      i0 = 0
      num_scalar_rtp = 0
      do i = 1, rj_fld%num_phys
        if (rj_fld%num_component(i) .eq. n_scalar) then
          i0 = i0 + 1
          num_scalar_rtp = num_scalar_rtp + 1
          phys_name_rtp(i0) =        rj_fld%phys_name(i)
        end if
      end do
      istart_scalar_rtp = 1
!
      num_vector_rtp = 0
      do i = 1, rj_fld%num_phys
        if (rj_fld%num_component(i) .eq. n_vector) then
          i0 = i0 + 1
          num_vector_rtp = num_vector_rtp + 1
          phys_name_rtp(i0) =        rj_fld%phys_name(i)
        end if
      end do
      istart_vector_rtp = istart_scalar_rtp + num_scalar_rtp
!
      num_tensor_rtp = 0
      do i = 1, rj_fld%num_phys
        if (rj_fld%num_component(i) .eq. n_sym_tensor) then
          i0 = i0 + 1
          num_tensor_rtp = num_tensor_rtp + 1
          phys_name_rtp(i0) =        rj_fld%phys_name(i)
        end if
      end do
      istart_tensor_rtp = istart_vector_rtp + num_vector_rtp
!
      if (iflag_debug .gt. 0) then
!        write(*,*) 'num_phys_rj', rj_fld%num_phys
!        write(*,*) 'id, components, stack, phys_name_rj'
!        do i = 1, rj_fld%num_phys
!          write(*,*) i, rj_fld%num_component(i),                       &
!     &        rj_fld%istack_component(i), trim(rj_fld%phys_name(i))
!        end do
        write(*,*)
        write(*,*) 'num_phys_rtp', num_phys_rtp
        write(*,*) 'phys_name_rtp', size(phys_name_rtp)
        write(*,*) 'id, components, stack, phys_name_rtp'
        do i = 1, num_phys_rtp
          write(*,*) i, trim(phys_name_rtp(i))
        end do
        write(*,*) 'istart_scalar_rtp',                                 &
     &              istart_scalar_rtp, num_scalar_rtp
        write(*,*) 'istart_vector_rtp',                                 &
     &              istart_vector_rtp, num_vector_rtp
        write(*,*) 'istart_tensor_rtp',                                 &
     &              istart_tensor_rtp, num_tensor_rtp
      end if
!
!
      end subroutine copy_sph_name_rj_to_rtp
!
! -------------------------------------------------------------------
!
      end module set_phys_name_4_sph_trans
 