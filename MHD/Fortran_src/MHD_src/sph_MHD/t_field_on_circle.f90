!>@file   t_field_on_circle.f90
!!@brief  module t_field_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine allocate_circle_field                                &
!!     &         (mphi_rtp, nidx_global_jmax, circle, d_circle)
!!      subroutine deallocate_circle_field(circle, d_circle)
!!
!!      subroutine write_field_data_on_circle                           &
!!     &         (i_step, time, circle, d_circle)
!!      subroutine read_field_data_on_circle                            &
!!     &         (i_step, time, ierr, circle, d_circle)
!!
!!      subroutine open_read_field_data_on_circle                       &
!!     &         (sph_rtp, sph_rj, circle, d_circle)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(fields_on_circle), intent(inout) :: d_circle
!!      subroutine close_field_data_on_circle
!!@endverbatim
!
      module t_field_on_circle
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
!
      implicit none
!
!
!>      file ID for field data on a circle
      integer(kind=kint), parameter :: id_circ_fid = 41
!>      file DI for spectr power data on a circle
      integer(kind=kint), parameter :: id_circ_sq =  42
!>      file ID for spectr phase data on a circle
      integer(kind=kint), parameter :: id_circ_ph =  43
!
!>      Structure to make fields on circle
      type fields_on_circle
!>        file name for field data on a circle
        character(len=kchara) :: fname_circle_fld = 'circle_field.dat'
!>        file name for spectr power data on a circle
        character(len=kchara) :: fname_circle_mag                       &
     &                        = 'circle_spec_mag.dat'
!>        file name for spectr phase data on a circle
        character(len=kchara) :: fname_circle_phs                       &
     &                        = 'circle_spec_phase.dat'
!
!>        cylindrical radius for a circle to pick
        real(kind = kreal) :: s_circle
!>        vartical position for a circle to pick
        real(kind = kreal) :: z_circle
!
!>        Inner closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_in
!>        Outer closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_out
!>        Inner closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_in
!>        Outer closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_out
!
!>        Spectr data for circle point for each domain
        real(kind = kreal), allocatable :: d_rj_circ_lc(:,:)
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: d_rj_circle(:,:)
!
!>        Field data for circle point at equator
        real(kind = kreal), allocatable :: v_rtp_circle(:,:)
!
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_mag(:,:)
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_phase(:,:)
      end type fields_on_circle
!
      type circle_fld_maker
!>        Structure to make fields on circle
        type(fields_on_circle) :: circle
!>         Structure of field data on circle
        type(phys_data) :: d_circle
      end type circle_fld_maker
!
      private :: id_circ_fid, id_circ_sq, id_circ_ph
      private :: open_field_data_on_circle
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_circle_field                                  &
     &         (mphi_rtp, nidx_global_jmax, circle, d_circle)
!
      use calypso_mpi
      use m_circle_transform
!
      integer(kind = kint), intent(in) :: mphi_rtp, nidx_global_jmax
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      integer(kind = kint) :: jmax_gl, ntot
!
!
      jmax_gl = nidx_global_jmax
      ntot = d_circle%ntot_phys
!
      if(mphi_circle .le. izero) mphi_circle = mphi_rtp
      allocate(circle%v_rtp_circle(mphi_circle,6))
      circle%v_rtp_circle = 0.0d0
!
      allocate( circle%vrtm_mag(0:mphi_circle,ntot) )
      allocate( circle%vrtm_phase(0:mphi_circle,ntot) )
      circle%vrtm_mag = 0.0d0
      circle%vrtm_phase = 0.0d0
!
      allocate( circle%d_rj_circ_lc(0:jmax_gl,ntot) )
      circle%d_rj_circ_lc = 0.0d0
!
      if(my_rank .eq. 0) then
        allocate(circle%d_rj_circle(0:jmax_gl,ntot) )
!
        circle%d_rj_circle = 0.0d0
      end if
!
      call alloc_phys_data_type(mphi_circle, d_circle)
!
      end subroutine allocate_circle_field
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_circle_field(circle, d_circle)
!
      use calypso_mpi
!
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
!
      deallocate(circle%vrtm_mag, circle%vrtm_phase)
      deallocate(circle%d_rj_circ_lc)
      if(my_rank .eq. 0) then
        deallocate(circle%d_rj_circle, circle%v_rtp_circle)
      end if
!
      call dealloc_phys_data_type(d_circle)
      call dealloc_phys_name_type(d_circle)
!
      end subroutine deallocate_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_field_data_on_circle                             &
     &         (i_step, time, circle, d_circle)
!
      use calypso_mpi
      use m_circle_transform
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(fields_on_circle), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: mphi
      real(kind = kreal) :: phi, amphi_circle
!
!
      if(my_rank .gt. 0) return
!
      amphi_circle = two*four*atan(one) / dble(mphi_circle)
!
      call open_field_data_on_circle(circle, d_circle)
!
      write(fmt_txt,'(a20,i5,a13)') '(i16,1pE25.15e3,i16,',             &
     &              (d_circle%ntot_phys_viz+1), '(1pE25.15e3))'
      do mphi = 1, mphi_circle
        phi = dble(mphi-1) * amphi_circle
        write(id_circ_fid,fmt_txt) i_step, time, mphi, phi,             &
     &             d_circle%d_fld(mphi,1:d_circle%ntot_phys_viz)
      end do
!
      write(fmt_txt,'(a20,i5,a13)') '(i16,1pE25.15e3,i16,',             &
     &              d_circle%ntot_phys_viz, '(1pE25.15e3))'
      do mphi = 0, mphi_circle / 2
        write(id_circ_sq,fmt_txt) i_step, time, mphi,                   &
     &             circle%vrtm_mag(mphi,1:d_circle%ntot_phys_viz)
        write(id_circ_ph,fmt_txt) i_step, time, mphi,                   &
     &             circle%vrtm_phase(mphi,1:d_circle%ntot_phys_viz)
      end do
!
      call close_field_data_on_circle
!
      end subroutine write_field_data_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine read_field_data_on_circle                              &
     &         (i_step, time, ierr, circle, d_circle)
!
      use m_circle_transform
!
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      integer(kind = kint) :: mphi, itmp
      real(kind = kreal) :: rtmp
!
!
      do mphi = 1, mphi_circle
        read(id_circ_fid,*,err=99,end=99) i_step, time, itmp, rtmp,     &
     &             d_circle%d_fld(mphi,1:d_circle%ntot_phys_viz)
      end do
!
      do mphi = 0, mphi_circle / 2
        read(id_circ_sq,*,err=99,end=99) i_step, time, itmp,            &
     &             circle%vrtm_mag(mphi,1:d_circle%ntot_phys_viz)
        read(id_circ_ph,*,err=99,end=99) i_step, time, itmp,            &
     &             circle%vrtm_phase(mphi,1:d_circle%ntot_phys_viz)
      end do
!
      ierr = 0
      return
!
  99  continue
      ierr = 1
      return
!
      end subroutine read_field_data_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine open_field_data_on_circle(circle, d_circle)
!
      use calypso_mpi
      use m_phys_constants
      use m_circle_transform
      use sel_comp_labels_by_coord
      use write_field_labels
!
      type(fields_on_circle), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      integer(kind = kint) :: ifld
      character(len=kchara) :: label(6)
!
!
      open(id_circ_fid, file=circle%fname_circle_fld,                   &
     &    form='formatted', status='old', position='append', err = 99)
      open(id_circ_sq,  file=circle%fname_circle_mag,                   &
     &    form='formatted', status='old', position='append', err = 98)
      open(id_circ_ph,  file=circle%fname_circle_phs,                   &
     &    form='formatted', status='old', position='append', err = 97)
!
      return
!
  97  continue
      close(id_circ_sq)
  98  continue
      close(id_circ_fid)
  99  continue
!
      open(id_circ_fid, file=circle%fname_circle_fld)
      open(id_circ_sq,  file=circle%fname_circle_mag)
      open(id_circ_ph,  file=circle%fname_circle_phs)
!
      write(id_circ_fid,'(a)') '#'
      write(id_circ_sq, '(a)') '#'
      write(id_circ_ph, '(a)') '#'
      write(id_circ_fid,'(a)') '# Cylindrical radius, vertial position'
      write(id_circ_sq, '(a)') '# Cylindrical radius, vertial position'
      write(id_circ_ph, '(a)') '# Cylindrical radius, vertial position'
      write(id_circ_fid,'(1p2e23.12)') circle%s_circle, circle%z_circle
      write(id_circ_sq, '(1p2e23.12)') circle%s_circle, circle%z_circle
      write(id_circ_ph, '(1p2e23.12)') circle%s_circle, circle%z_circle
!
      write(id_circ_fid,'(a)') '#'
      write(id_circ_sq, '(a)') '#'
      write(id_circ_ph, '(a)') '#'
      write(id_circ_fid,'(a)') '# Number of points and components'
      write(id_circ_sq, '(a)') '# Number of modes and components'
      write(id_circ_ph, '(a)') '# Number of modes and components'
      write(id_circ_fid,'(2i16)') mphi_circle, d_circle%ntot_phys_viz
      write(id_circ_sq, '(2i16)') mphi_circle/2, d_circle%ntot_phys_viz
      write(id_circ_ph, '(2i16)') mphi_circle/2, d_circle%ntot_phys_viz
!
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
      subroutine open_read_field_data_on_circle                         &
     &         (sph_rtp, sph_rj, circle, d_circle)
!
      use m_circle_transform
      use skip_comment_f
!
      use t_spheric_rtp_data
      use t_spheric_rj_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      character(len=255) :: tmpchara
      character(len=kchara) :: phi_name
!
!
      open(id_circ_fid, file=circle%fname_circle_fld)
      open(id_circ_sq,  file=circle%fname_circle_mag)
      open(id_circ_ph,  file=circle%fname_circle_phs)
!
      call skip_comment(tmpchara, id_circ_fid)
      read(tmpchara,*) circle%s_circle, circle%z_circle
      call skip_comment(tmpchara, id_circ_fid)
      read(tmpchara,*) mphi_circle, d_circle%ntot_phys_viz
!
      call skip_comment(tmpchara, id_circ_sq)
      call skip_comment(tmpchara, id_circ_ph)
      call skip_comment(tmpchara, id_circ_sq)
      call skip_comment(tmpchara, id_circ_ph)
!
      d_circle%num_phys_viz = d_circle%ntot_phys_viz
      d_circle%num_phys =     d_circle%ntot_phys_viz
      d_circle%ntot_phys =    d_circle%ntot_phys_viz
!
      write(*,*) 'alloc_phys_name_type'
      call alloc_phys_name_type(d_circle)
      write(*,*) 'allocate_circle_field'
      call allocate_circle_field                                        &
     &   (sph_rtp%nidx_rtp(3), sph_rj%nidx_global_rj(2),                &
     &    circle, d_circle)
!
      d_circle%num_component = 1
!
      write(*,*) 'read field name', size(d_circle%phys_name),           &
     &          d_circle%num_phys
      read(id_circ_fid,*) tmpchara, tmpchara, tmpchara, phi_name,       &
     &                  d_circle%phys_name(1:d_circle%num_phys)
      read(id_circ_sq,*) tmpchara, tmpchara, tmpchara,                  &
     &                  d_circle%phys_name(1:d_circle%num_phys)
      read(id_circ_ph,*) tmpchara, tmpchara, tmpchara,                  &
     &                  d_circle%phys_name(1:d_circle%num_phys)
!
      end subroutine open_read_field_data_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine close_field_data_on_circle
!
      use calypso_mpi
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
      end module t_field_on_circle
