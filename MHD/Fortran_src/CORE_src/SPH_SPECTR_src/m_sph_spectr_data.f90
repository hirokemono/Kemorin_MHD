!>@file   m_sph_spectr_data.f90
!!@brief  module m_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Flag and parameters for spherical transform dnyamo model
!!
!!
!!@verbatim
!!      subroutine allocate_reft_rj_data
!!
!!      subroutine deallocate_reft_rj_data
!!
!!      subroutine check_rj_spectr_name
!!      subroutine check_rj_spectr_data(my_rank)
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module m_sph_spectr_data
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
!>        Structure for field data
        type(phys_data) :: rj_fld1
!
!>      Number of fields of scalar fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_scalar_rtp
!>      Number of fields of vector fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_vector_rtp
!>      Number of fields of tensor fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_tensor_rtp
!
!>    reference temerature spectr @f$ f(r,j) @f$
!!@verbatim
!!        reftemp_rj(kr,0) ... T_0
!!        reftemp_rj(kr,1) ... d T_0 / dr
!!@endverbatim
      real (kind=kreal), allocatable :: reftemp_rj(:,:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_reft_rj_data
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
      num = nidx_rj(1)
      allocate( reftemp_rj(num,0:1)   )
      reftemp_rj =  0.0d0
!
      end subroutine allocate_reft_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine deallocate_reft_rj_data
!
      deallocate( reftemp_rj )
!
      end subroutine deallocate_reft_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_name
!
      integer(kind = kint) :: i
!
!
      write(*,'(a,i16)') 'num_phys_rj ', rj_fld1%num_phys
      write(*,'(a)') 'number, component, stack, monitor_flag, name'
      do i = 1, rj_fld1%num_phys
        write(*,'(4i6,a2,a)') i, rj_fld1%num_component(i),              &
     &           rj_fld1%istack_component(i), rj_fld1%iflag_monitor(i), &
     &           '  ', trim(rj_fld1%phys_name(i))
      end do
!
      end subroutine check_rj_spectr_name
!
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_data(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: inod
      character(len=kchara) :: fmt_txt
!
      write(50+my_rank,*) 'num_phys_rj',  rj_fld1%num_phys
      write(50+my_rank,*) 'ntot_phys_rj', rj_fld1%ntot_phys
      write(50+my_rank,*) 'num_phys_comp_rj', rj_fld1%num_component
      do inod = 1, rj_fld1%num_phys
        write(50+my_rank,*) rj_fld1%phys_name(inod)
      end do
      write(fmt_txt,'(a6,i3,a16)')                                      &
     &           '(3i16,', rj_fld1%ntot_phys, '(1pE25.15e3),a1)'
      do inod = 1, nnod_rj
        write(50+my_rank,fmt_txt) inod, idx_global_rj(inod,1:2),        &
     &    rj_fld1%d_fld(inod,1:rj_fld1%ntot_phys)
      end do
!
!
      end subroutine check_rj_spectr_data
!
!  --------------------------------------------------------------------
!
      end module m_sph_spectr_data
