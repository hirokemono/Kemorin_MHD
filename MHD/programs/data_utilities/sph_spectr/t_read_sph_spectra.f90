!>@file   t_read_sph_spectra.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine dealloc_sph_espec_data(sph_IN)
!!
!!      subroutine input_sph_pwr_vol_head(id_file, sph_IN)
!!      subroutine input_sph_spectr_vol_head(id_file, sph_IN)
!!      subroutine input_sph_pwr_layer_head(id_file, sph_IN)
!!      subroutine input_sph_spectr_layer_head(id_file, sph_IN)
!!
!!      subroutine input_sph_pwr_vol_head_old(id_file, sph_IN)
!!      subroutine input_sph_spectr_vol_head_old(id_file, sph_IN)
!!      subroutine input_sph_pwr_layer_head_old(id_file, sph_IN)
!!      subroutine input_sph_spectr_layer_head_old(id_file, sph_IN)
!!
!!      subroutine write_sph_pwr_vol_head(id_file, sph_IN)
!!      subroutine write_sph_pwr_layer_head(id_file, sph_IN)
!!@endverbatim
!
      module t_read_sph_spectra
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type read_sph_spectr_data
        integer(kind = kint) :: iflag_vol_ave
        integer(kind = kint) :: iflag_spectr
        integer(kind = kint) :: iflag_old_fmt
!
        integer(kind = kint) :: nfield_sph_spec
        integer(kind = kint) :: ntot_sph_spec
        integer(kind = kint) :: num_time_labels
        integer(kind = kint) :: num_labels
        integer(kind = kint), allocatable :: ncomp_sph_spec(:)
        character(len = kchara), allocatable :: ene_sph_spec_name(:)
!
        integer(kind = kint) :: ltr_sph, nri_sph
        integer(kind = kint) :: kr_ICB, kr_CMB
        integer(kind = kint) :: kr_inner, kr_outer
        integer(kind = kint), allocatable :: kr_sph(:)
        real(kind = kreal), allocatable :: r_sph(:)
        real(kind = kreal), allocatable :: r_inner, r_outer
!
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
        real(kind = kreal), allocatable :: spectr_IO(:,:,:)
      end type read_sph_spectr_data
!
      private :: alloc_sph_espec_name, alloc_sph_spectr_data
      private :: read_sph_pwr_vol_head, read_sph_pwr_layer_head
      private :: read_sph_spectr_name
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_espec_name(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      sph_IN%num_labels = sph_IN%ntot_sph_spec + sph_IN%num_time_labels
      allocate( sph_IN%ene_sph_spec_name(sph_IN%num_labels) )
      allocate( sph_IN%ncomp_sph_spec(sph_IN%nfield_sph_spec))
      sph_IN%ncomp_sph_spec = 0
!
      end subroutine alloc_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_spectr_data(ltr, sph_IN)
!
      integer(kind = kint), intent(in) :: ltr
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: ncomp
!
!
      allocate( sph_IN%kr_sph(sph_IN%nri_sph) )
      allocate( sph_IN%r_sph(sph_IN%nri_sph) )
!
      ncomp = sph_IN%ntot_sph_spec
      allocate( sph_IN%spectr_IO(ncomp,0:ltr,sph_IN%nri_sph) )
!
      sph_IN%kr_sph = izero
      sph_IN%r_sph = zero
      sph_IN%spectr_IO =  zero
!
      end subroutine alloc_sph_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_espec_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      deallocate(sph_IN%ene_sph_spec_name, sph_IN%ncomp_sph_spec)
      deallocate(sph_IN%kr_sph, sph_IN%r_sph, sph_IN%spectr_IO)
!
      end subroutine dealloc_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_sph_pwr_vol_head(id_file, sph_IN)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nri_sph, sph_IN%ltr_sph
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_inner, sph_IN%r_inner
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_outer, sph_IN%r_outer
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
!
      end subroutine read_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_pwr_layer_head(id_file, sph_IN)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nri_sph, sph_IN%ltr_sph
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
!
      end subroutine read_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_vol_head(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') sph_IN%nri_sph, sph_IN%ltr_sph
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') sph_IN%kr_ICB, sph_IN%kr_CMB
      write(id_file,'(a)')    'Lower boudary'
      write(id_file,'(i16,1pe23.14e3)') sph_IN%kr_inner, sph_IN%r_inner
      write(id_file,'(a)')    'Upper boundary'
      write(id_file,'(i16,1pe23.14e3)') sph_IN%kr_outer, sph_IN%r_outer
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')                                           &
     &      sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
!
      do i = 1, sph_IN%num_labels
        write(id_file,'(2a)',advance='no')                              &
     &            trim(sph_IN%ene_sph_spec_name(i)), '    '
      end  do
      write(id_file,*)
!
      end subroutine write_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_layer_head(id_file, sph_IN)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') sph_IN%nri_sph, sph_IN%ltr_sph
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') sph_IN%kr_ICB, sph_IN%kr_CMB
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')                                           &
     &      sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
!
      do i = 1, sph_IN%num_labels
        write(id_file,'(2a)',advance='no')                              &
     &            trim(sph_IN%ene_sph_spec_name(i)), '    '
      end  do
      write(id_file,*)
!
      end subroutine write_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_sph_spectr_name(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint) :: i
!
!
      read(id_file,*) sph_IN%ncomp_sph_spec(1:sph_IN%nfield_sph_spec)
      read(id_file,*)  sph_IN%ene_sph_spec_name(1:sph_IN%num_labels)
      do i = 1, sph_IN%num_labels
        write(*,*) i, trim(sph_IN%ene_sph_spec_name(i))
      end  do
!
      end subroutine read_sph_spectr_name
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine input_sph_pwr_vol_head(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_vol_head(id_file, sph_IN)
!
      sph_IN%nri_sph = 1
      sph_IN%num_time_labels = 2
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(izero, sph_IN)
!
      end subroutine input_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine input_sph_spectr_vol_head(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_vol_head(id_file, sph_IN)
!
      sph_IN%nri_sph = 1
      sph_IN%num_time_labels = 3
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      end subroutine input_sph_spectr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine input_sph_pwr_layer_head(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_layer_head(id_file, sph_IN)
!
      sph_IN%num_time_labels = 4
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(izero, sph_IN)
!
      end subroutine input_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      subroutine input_sph_spectr_layer_head(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_layer_head(id_file, sph_IN)
!
      sph_IN%num_time_labels = 5
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      end subroutine input_sph_spectr_layer_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine input_sph_pwr_vol_head_old(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_layer_head(id_file, sph_IN)
!
      sph_IN%nri_sph = 1
      sph_IN%num_time_labels = 2
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(izero, sph_IN)
!
      end subroutine input_sph_pwr_vol_head_old
!
!   --------------------------------------------------------------------
!
      subroutine input_sph_spectr_vol_head_old(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_layer_head(id_file, sph_IN)
!
      sph_IN%nri_sph = 1
      sph_IN%num_time_labels = 3
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      end subroutine input_sph_spectr_vol_head_old
!
!   --------------------------------------------------------------------
!
      subroutine input_sph_pwr_layer_head_old(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_layer_head(id_file, sph_IN)
!
      sph_IN%num_time_labels = 3
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(izero, sph_IN)
!
      end subroutine input_sph_pwr_layer_head_old
!
!   --------------------------------------------------------------------
!
      subroutine input_sph_spectr_layer_head_old(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call read_sph_pwr_layer_head(id_file, sph_IN)
!
      sph_IN%num_time_labels = 4
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name(id_file, sph_IN)
!
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      end subroutine input_sph_spectr_layer_head_old
!
!   --------------------------------------------------------------------
!
      end module t_read_sph_spectra
