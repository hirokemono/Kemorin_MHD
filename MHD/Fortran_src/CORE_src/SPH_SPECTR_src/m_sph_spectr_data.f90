!
!     module m_sph_spectr_data
!
!      Written by H. Matsui on Oct., 2007
!
!       subroutine allocate_phys_rtp_name
!       subroutine allocate_phys_rj_name
!
!       subroutine allocate_phys_rtp_data
!       subroutine allocate_phys_rj_data
!
!       subroutine allocate_rot_rj_data
!       subroutine allocate_reft_rj_data
!
!       subroutine deallocate_phys_rtp_data
!       subroutine deallocate_phys_rj_data
!
!       subroutine deallocate_rot_rj_data
!       subroutine deallocate_reft_rj_data
!
!      subroutine check_rtp_phys_data(my_rank)
!      subroutine check_rj_spectr_name
!      subroutine check_rj_spectr_data(my_rank)
!
!
      module m_sph_spectr_data
!
      use m_precision
!
      implicit  none
! 
!
      integer (kind=kint) :: num_phys_rtp
      integer (kind=kint) :: num_phys_rj
!    number of physical data
      integer (kind=kint) :: ntot_phys_rtp
      integer (kind=kint) :: ntot_phys_rj
!
      integer (kind=kint) :: num_phys_rj_vis
      integer (kind=kint) :: ntot_comp_rj_vis
!    number of physical data for output
!
      integer (kind=kint), allocatable:: iflag_monitor_rtp(:)
      integer (kind=kint), allocatable:: iflag_monitor_rj(:)
!
      integer (kind=kint), allocatable :: num_phys_comp_rtp(:)
      integer (kind=kint), allocatable :: num_phys_comp_rj(:)
! 
      integer (kind=kint), allocatable :: istack_phys_comp_rtp(:)
      integer (kind=kint), allocatable :: istack_phys_comp_rj(:)
! 
      character (len=kchara), allocatable :: phys_name_rtp(:)
      character (len=kchara), allocatable :: phys_name_rj(:)
! 
      real (kind=kreal), allocatable :: d_rj(:,:)
      real (kind=kreal), allocatable :: d_rtp(:,:)
!
      integer (kind=kint) :: istart_scalar_rtp, num_scalar_rtp
      integer (kind=kint) :: istart_vector_rtp, num_vector_rtp
      integer (kind=kint) :: istart_grad_v_rtp, num_grad_v_rtp
      integer (kind=kint) :: istart_tensor_rtp, num_tensor_rtp
!
!     rotation spectr
!
!        omega(k,0,1) ... Omaga_x
!        omega(k,1,1) ... d Omaga_x / dr
!        omega(k,2,1) ... d^2 Omaga_x / dr^2
!        omega(k,0,2) ... Omaga_z
!        omega(k,1,2) ... d Omaga_z / dr
!        omega(k,2,2) ... d^2 Omaga_z / dr^2
!        omega(k,0,3) ... Omaga_y
!        omega(k,1,3) ... d Omaga_y / dr
!        omega(k,2,3) ... d^2 Omaga_y / dr^2
      real(kind = kreal), allocatable :: omega_rj(:,:,:)
      real(kind = kreal), allocatable :: omega_rlm(:,:,:)
!
!     reference temerature spectr
!
      real (kind=kreal), allocatable :: reftemp_rj(:,:)
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
      allocate( iflag_monitor_rtp(num_phys_rtp) )
      allocate( num_phys_comp_rtp(num_phys_rtp) )
      allocate( istack_phys_comp_rtp(0:num_phys_rtp) )
!
      if(num_phys_rtp .gt. 0) then
        phys_name_rtp = ''
        iflag_monitor_rtp =    0
        num_phys_comp_rtp =    0
      end if
      istack_phys_comp_rtp = 0
!
      end subroutine allocate_phys_rtp_name
!
!  --------------------------------------------------------------------
!
      subroutine allocate_phys_rj_name
!
      allocate( phys_name_rj(num_phys_rj) )
      allocate( iflag_monitor_rj(num_phys_rj) )
      allocate( num_phys_comp_rj(num_phys_rj) )
      allocate( istack_phys_comp_rj(0:num_phys_rj) )
!
      if(num_phys_rj .gt. 0) then
        phys_name_rj = ''
        iflag_monitor_rj =    0
        num_phys_comp_rj =    0
      end if
      istack_phys_comp_rj = 0
!
      end subroutine allocate_phys_rj_name
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine allocate_phys_rtp_data
!
      use m_spheric_parameter
!
      allocate( d_rtp(nnod_rtp,ntot_phys_rtp) )
      if((nnod_rtp*ntot_phys_rtp) .gt. 0) d_rtp = 0.0d0
!
      end subroutine allocate_phys_rtp_data
!
!  --------------------------------------------------------------------
!
      subroutine allocate_phys_rj_data
!
      use m_spheric_parameter
!
      allocate( d_rj(nnod_rj,ntot_phys_rj) )
      if((nnod_rj*ntot_phys_rj) .gt. 0) d_rj = 0.0d0
!
      end subroutine allocate_phys_rj_data
!
!  --------------------------------------------------------------------
!
      subroutine allocate_rot_rj_data
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
      num = nidx_rj(1)
      allocate( omega_rj(num,0:2,3) )
      num = nidx_rlm(1)
      allocate( omega_rlm(num,0:2,3) )
      omega_rj =  0.0d0
      omega_rlm = 0.0d0
!
      end subroutine allocate_rot_rj_data
!
!  --------------------------------------------------------------------
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
      subroutine deallocate_phys_rtp_data
!
      deallocate( phys_name_rtp )
      deallocate( iflag_monitor_rtp )
      deallocate( num_phys_comp_rtp )
      deallocate( istack_phys_comp_rtp )
      deallocate( d_rtp )
!
      end subroutine deallocate_phys_rtp_data
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_phys_rj_data
!
      deallocate( phys_name_rj )
      deallocate( iflag_monitor_rj )
      deallocate( num_phys_comp_rj )
      deallocate( istack_phys_comp_rj )
      deallocate( d_rj )
!
      end subroutine deallocate_phys_rj_data
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_rot_rj_data
!
      deallocate(omega_rj)
!
      end subroutine deallocate_rot_rj_data
!
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
      subroutine check_rtp_phys_data(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: inod
      character(len=kchara) :: fmt_txt
!
      write(50+my_rank,*) 'num_phys_rtp',  num_phys_rtp
      write(50+my_rank,*) 'ntot_phys_rtp', ntot_phys_rtp
      write(50+my_rank,*) 'num_phys_comp_rtp', num_phys_comp_rtp
      do inod = 1, num_phys_rtp
        write(50+my_rank,*) phys_name_rtp(inod)
      end do
!
      write(fmt_txt,'(a6,i3,a16)')                                      &
     &           '(4i10,', ntot_phys_rtp, '(1pE25.15e3),a1)'
      do inod = 1, nnod_rtp
        write(50+my_rank,fmt_txt) inod,                                 &
     &        idx_global_rtp(inod,1:3), d_rtp(inod,1:ntot_phys_rtp)
      end do
!
!
      end subroutine check_rtp_phys_data
!
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_name
!
      integer(kind = kint) :: i
!
!
      write(*,'(a,i10)') 'num_phys_rj ', num_phys_rj
      write(*,'(a)') 'number, component, stack, monitor_flag, name'
      do i = 1, num_phys_rj
        write(*,'(4i6,a)') i, num_phys_comp_rj(i),                      &
     &                     istack_phys_comp_rj(i), iflag_monitor_rj(i), &
     &                     trim(phys_name_rj(i))
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
      write(50+my_rank,*) 'num_phys_rj',  num_phys_rj
      write(50+my_rank,*) 'ntot_phys_rj', ntot_phys_rj
      write(50+my_rank,*) 'num_phys_comp_rj', num_phys_comp_rj
      do inod = 1, num_phys_rj
        write(50+my_rank,*) phys_name_rj(inod)
      end do
      write(fmt_txt,'(a6,i3,a16)')                                      &
     &           '(3i10,', ntot_phys_rj, '(1pE25.15e3),a1)'
      do inod = 1, nnod_rj
        write(50+my_rank,fmt_txt) inod,                                 &
     &        idx_global_rj(inod,1:2), d_rj(inod,1:ntot_phys_rj)
      end do
!
!
      end subroutine check_rj_spectr_data
!
!  --------------------------------------------------------------------
!
      end module m_sph_spectr_data
