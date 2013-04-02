!
!      module m_correlate_4_plane
!
!      Written by Kemorin
!
!       subroutine allocate_correlate_name
!       subroutine allocate_correlate_4_plane
!       subroutine allocate_correlate_4_evo
!       subroutine allocate_correlate_4_snap
!
!       subroutine open_correlate_files_plane
!       subroutine open_correlate_files_snap(istep)
!       subroutine close_results_4_correlate
!
      module m_correlate_4_plane
!
      use m_precision
!
      use m_file_format_switch
      use m_field_file_format
      use t_phys_data
!
      implicit    none
!
      integer(kind=kint ) :: num_crt, num_domain
!
      character(len = kchara) :: cor_mesh_header = "mesh/in"
      character(len = kchara) :: ref_mesh_header = "mesh_ref/in"
      integer (kind = kint) :: id_cor_mesh_fmt = id_ascii_file_fmt
      integer (kind = kint) :: id_ref_mesh_fmt = id_ascii_file_fmt
!
      character(len=kchara) :: cor_udt_header =  'field/out'
      character(len=kchara) :: ref_udt_header =  'field_ref/out'
!
      integer (kind = kint) :: itype_cor_ucd_file = iflag_fld
      integer (kind = kint) :: itype_ref_ucd_file = iflag_fld
!
      type(phys_data) :: cor_ucd
      type(phys_data) :: ref_ucd
!
!
      real   (kind=kreal), allocatable  ::  phys_d1(:)
      real   (kind=kreal), allocatable  ::  phys_d2(:)
      integer(kind=kint ) :: kx_max, ky_max, iz_max
!      real   (kind=kreal), allocatable  ::  phys_io

      character(len=kchara), allocatable :: crt_name(:)
      character(len=kchara), allocatable :: crt_comp(:)
      integer(kind=kint ), allocatable :: ifield_crt(:)
      integer(kind=kint ), allocatable :: ifield_crt2(:)
      integer(kind=kint ), allocatable :: icomp_crt(:)

      real   (kind=kreal), allocatable  ::  ave_data(:)
      real   (kind=kreal), allocatable  ::  ave_data2(:)
      real   (kind=kreal), allocatable  ::  rms_data(:)
      real   (kind=kreal), allocatable  ::  rms_data2(:)
      real   (kind=kreal), allocatable  ::  sig_data(:)
      real   (kind=kreal), allocatable  ::  sig_data2(:)
      real   (kind=kreal), allocatable  ::  crt_data(:)
      real   (kind=kreal), allocatable  ::  rms_ratio(:)
      real   (kind=kreal), allocatable  ::  x_out(:)
      real   (kind=kreal), allocatable  ::  y_out(:)
      real   (kind=kreal), allocatable  ::  z_out(:)
!
      integer(kind=kint ), parameter :: crt_data_code = 25
      integer(kind=kint ), parameter :: rms_data_code = 27
      character(len=kchara) :: crt_rst_name
      character(len=kchara) :: rms_rst_name
!
      character(len=kchara), parameter :: crt_rst_header = 'correlate'
      character(len=kchara), parameter :: rms_rst_header = 'rms_ratio'
!
      private :: crt_rst_header, rms_rst_header
      private :: crt_rst_name, rms_rst_name
!
      private :: write_header_4_correlate_snap
      private :: write_header_4_correlate_plane
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_correlate_name
!
!
        allocate( crt_name(num_crt) )
        allocate( crt_comp(num_crt) )
        allocate( ifield_crt(num_crt) )
        allocate( ifield_crt2(num_crt) )
        allocate( icomp_crt(num_crt) )
!
        ifield_crt = 0
        ifield_crt2 = 0
        icomp_crt = 0
!
!
       end subroutine allocate_correlate_name
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_correlate_4_plane
!
!
       allocate ( phys_d1(num_domain*num_crt) )
       allocate ( phys_d2(num_domain*num_crt) )
       allocate ( ave_data(iz_max*num_crt) )
       allocate ( ave_data2(iz_max*num_crt) )
       allocate ( rms_data(iz_max*num_crt) )
       allocate ( rms_data2(iz_max*num_crt) )
       allocate ( sig_data(iz_max*num_crt) )
       allocate ( sig_data2(iz_max*num_crt) )
        allocate( x_out(kx_max) )
        allocate( y_out(ky_max) )
        allocate( z_out(iz_max) )

       phys_d1 = 0.0d0
       phys_d2 = 0.0d0
       ave_data = 0.0d0
       ave_data2 = 0.0d0
       rms_data = 0.0d0
       rms_data2 = 0.0d0
       sig_data = 0.0d0
       sig_data2 = 0.0d0
       x_out = 0.0d0
       y_out = 0.0d0
       z_out = 0.0d0
!
       end subroutine allocate_correlate_4_plane
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_correlate_4_evo
!
       allocate ( crt_data(iz_max*num_crt) )
       allocate ( rms_ratio(iz_max*num_crt) )
!
       crt_data =  0.0d0
       rms_ratio = 0.0d0
!
       end subroutine allocate_correlate_4_evo
!
!  ---------------------------------------------------------------------
!
      subroutine open_correlate_files_plane
!
      use set_parallel_file_name
!
      call add_dat_extension(crt_rst_header, crt_rst_name)
      call add_dat_extension(rms_rst_header, rms_rst_name)
!
      open (crt_data_code, file=crt_rst_name)
      open (rms_data_code, file=rms_rst_name)
!
      call write_header_4_correlate_plane(crt_data_code)
      call write_header_4_correlate_plane(rms_data_code)
!
      end subroutine open_correlate_files_plane
!
!  ---------------------------------------------------------------------
!
      subroutine open_correlate_files_snap(istep)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep
      character(len=kchara) :: fname_tmp1, fname_tmp2
!
      call add_int_suffix(istep, crt_rst_header, fname_tmp1)
      call add_int_suffix(istep, rms_rst_header, fname_tmp2)
!
      call add_dat_extension(fname_tmp1, crt_rst_name)
      call add_dat_extension(fname_tmp2, rms_rst_name)
!
      open (crt_data_code, file=crt_rst_name)
      open (rms_data_code, file=rms_rst_name)
!
      call write_header_4_correlate_snap(crt_data_code)
      call write_header_4_correlate_snap(rms_data_code)
!
       end subroutine open_correlate_files_snap
!
!  ---------------------------------------------------------------------
!
       subroutine write_header_4_correlate_plane(file_id)
!
       integer(kind = kint), intent(in) :: file_id
       integer(kind = kint) :: j
!
        write(file_id,*) 'number of component'
        write(file_id,*) num_crt
        write(file_id,*) 'step  iz  zz '
        do j = 1, num_crt
         write(file_id,*) trim(crt_name(j)), '_', trim(crt_comp(j))
        end do
!
       end subroutine write_header_4_correlate_plane
!
!  ---------------------------------------------------------------------
!
!
       subroutine write_header_4_correlate_snap(file_id)
!
       integer(kind = kint), intent(in) :: file_id
       integer(kind = kint) :: j
!
        write(file_id,*) 'number of component'
        write(file_id,*) num_crt
        write(file_id,*) 'step  ix iy iz  xx yy zz'
        do j = 1, num_crt
         write(file_id,*) trim(crt_name(j)), '_', trim(crt_comp(j))
        end do
!
       end subroutine write_header_4_correlate_snap
!
!  ---------------------------------------------------------------------
!
       subroutine close_results_4_correlate
!
       close( crt_data_code )
       close( rms_data_code )
!
       end subroutine close_results_4_correlate
!
!  ---------------------------------------------------------------------
!
      end module m_correlate_4_plane
