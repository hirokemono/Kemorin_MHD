!>@file   m_field_on_circle.f90
!!@brief  module m_field_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine allocate_circle_field
!!      subroutine deallocate_circle_field
!!
!!      subroutine write_field_data_on_circle(i_step, time)
!!      subroutine open_field_data_on_circle
!!      subroutine close_field_data_on_circle
!!@endverbatim
!
      module m_field_on_circle
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
      use m_machine_parameter
      use m_spheric_parameter
!
      use t_phys_data
!
      implicit none
!
!>      cylindrical radius for a circle to pick
      real(kind = kreal) :: s_circle
!>      vartical position for a circle to pick
      real(kind = kreal) :: z_circle
!
!>      Inner closest point of circle point of fluid shell
      integer(kind = kint) :: kr_gl_rcirc_in
!>      Outer closest point of circle point of fluid shell
      integer(kind = kint) :: kr_gl_rcirc_out
!>      Inner closest radius of circle point of fluid shell
      real(kind = kreal) :: coef_gl_rcirc_in
!>      Outer closest radius of circle point of fluid shell
      real(kind = kreal) :: coef_gl_rcirc_out
!
!>      Spectr data for circle point for each domain
      real(kind = kreal), allocatable :: d_rj_circ_lc(:,:)
!>      Spectr data for circle point collected to 0 process
      real(kind = kreal), allocatable :: d_rj_circle(:,:)
!
!>      Field data for circle point at equator
      real(kind = kreal), allocatable :: v_rtp_circle(:,:)
!
!>      Spectr data for circle point collected to 0 process
      real(kind = kreal), allocatable :: vrtm_sqare(:,:)
!>      Spectr data for circle point collected to 0 process
      real(kind = kreal), allocatable :: vrtm_phase(:,:)
!
!>       Structure for field data on circle
      type(phys_data), save :: d_circle
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_circle_field
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_circle_transform
!
      integer(kind = kint) :: num
!
!
      if(mphi_circle .le. izero) mphi_circle = nidx_rtp(3)
      allocate(v_rtp_circle(mphi_circle,6))
      v_rtp_circle = 0.0d0
!
      allocate( vrtm_sqare(0:mphi_circle,d_circle%ntot_phys) )
      allocate( vrtm_phase(0:mphi_circle,d_circle%ntot_phys) )
!
      num = nidx_global_rj(2)
      allocate( d_rj_circ_lc(0:num,d_circle%ntot_phys) )
      d_rj_circ_lc = 0.0d0
!
      if(my_rank .eq. 0) then
        num = nidx_global_rj(2)
        allocate( d_rj_circle(0:num,d_circle%ntot_phys) )
!
        d_rj_circle = 0.0d0
      end if
!
      call alloc_phys_data_type(mphi_circle, d_circle)
!
      end subroutine allocate_circle_field
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_circle_field
!
!
      deallocate(vrtm_sqare, vrtm_phase)
      deallocate(d_rj_circ_lc)
      if(my_rank .eq. 0) deallocate(d_rj_circle, v_rtp_circle)
!
      end subroutine deallocate_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_field_data_on_circle(i_step, time)
!
      use m_file_control_parameter
      use m_circle_transform
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      character(len=kchara) :: fmt_txt
!
      integer(kind = kint) :: mphi
      real(kind = kreal) :: phi, amphi_circle
!
!
      if(my_rank .gt. 0) return
!
      amphi_circle = two*four*atan(one) / dble(mphi_circle)
!
      write(fmt_txt,'(a20,i5,a13)') '(i10,1pE25.15e3,i10,',             &
     &              (d_circle%ntot_phys_viz+1), '(1pE25.15e3))'
      do mphi = 1, mphi_circle
        phi = dble(mphi-1) * amphi_circle
        write(id_circ_fid,fmt_txt) i_step, time, mphi, phi,             &
     &             d_circle%d_fld(mphi,1:d_circle%ntot_phys_viz)
      end do
!
      write(fmt_txt,'(a20,i5,a13)') '(i10,1pE25.15e3,i10,',             &
     &              d_circle%ntot_phys_viz, '(1pE25.15e3))'
      do mphi = 0, mphi_circle / 2
        write(id_circ_sq,fmt_txt) i_step, time, mphi,                   &
     &             vrtm_sqare(mphi,1:d_circle%ntot_phys_viz)
        write(id_circ_ph,fmt_txt) i_step, time, mphi,                   &
     &             vrtm_phase(mphi,1:d_circle%ntot_phys_viz)
      end do
!
      end subroutine write_field_data_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine open_field_data_on_circle
!
      use m_file_control_parameter
      use m_phys_constants
      use m_circle_transform
      use sel_comp_labels_by_coord
      use write_field_labels
!
      integer(kind = kint) :: ifld
      character(len=kchara) :: label(6)
!
!
      if(my_rank .gt. 0) return
!
      open(id_circ_fid, file='circle_field.dat')
      open(id_circ_sq,  file='circle_spec_square.dat')
      open(id_circ_ph,  file='circle_spec_phase.dat')
!
      write(label(1),'(a)') 't_step'
      call write_one_label(id_circ_fid, label(1))
      call write_one_label(id_circ_sq, label(1))
      call write_one_label(id_circ_ph, label(1))
      write(label(1),'(a)') 'time'
      call write_one_label(id_circ_fid, label(1))
      call write_one_label(id_circ_sq, label(1))
      call write_one_label(id_circ_ph, label(1))
!
      write(label(1),'(a)') 'mphi'
      call write_one_label(id_circ_fid, label(1))
      write(label(1),'(a)') 'longitude'
      call write_one_label(id_circ_fid, label(1))
!
      write(label(1),'(a)') 'order'
      call write_one_label(id_circ_sq, label(1))
      call write_one_label(id_circ_ph, label(1))
!
!
      do ifld = 1, d_circle%num_phys_viz
        if(d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call sel_coord_tensor_comp_labels(iflag_circle_coord,         &
     &        d_circle%phys_name(ifld), label(1) )
          call write_sym_tensor_label(id_circ_fid, label(1))
          call write_sym_tensor_label(id_circ_sq, label(1))
          call write_sym_tensor_label(id_circ_ph, label(1))
        else if(d_circle%num_component(ifld) .eq. n_vector) then
          call sel_coord_vector_comp_labels(iflag_circle_coord,         &
     &        d_circle%phys_name(ifld), label(1) )
          call write_vector_label(id_circ_fid, label(1))
          call write_vector_label(id_circ_sq, label(1))
          call write_vector_label(id_circ_ph, label(1))
        else
          write(label(1),'(a)') trim(d_circle%phys_name(ifld))
          call write_one_label(id_circ_fid, label(1))
          call write_one_label(id_circ_sq, label(1))
          call write_one_label(id_circ_ph, label(1))
        end if
      end do
      write(id_circ_fid,*)
      write(id_circ_sq,*)
      write(id_circ_ph,*)
!
      end subroutine open_field_data_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine close_field_data_on_circle
!
      use m_file_control_parameter
!
!
      if(my_rank .gt. 0) return
!
      close(id_circ_fid)
      close(id_circ_sq)
      close(id_circ_ph)
!
      end subroutine close_field_data_on_circle
!
! ----------------------------------------------------------------------
!
      end module m_field_on_circle
