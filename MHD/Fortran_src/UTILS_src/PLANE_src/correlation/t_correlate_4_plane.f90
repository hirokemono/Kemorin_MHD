!
!      module t_correlate_4_plane
!
!      Written by Kemorin
!
!       subroutine alloc_correlate_name
!       subroutine alloc_correlate_4_plane
!       subroutine alloc_correlate_4_evo
!
!       subroutine open_correlate_files_plane
!       subroutine open_correlate_files_snap(istep)
!       subroutine close_results_4_correlate
!
      module t_correlate_4_plane
!
      use m_precision
!
      use m_file_format_switch
      use m_field_file_format
      use t_file_IO_parameter
!
      implicit    none
!
      type correlate_4_plane
        integer(kind=kint ) :: num_crt, num_domain_c
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
      end type correlate_4_plane
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_correlate_name(pcor)
!
      type(correlate_4_plane), intent(inout) :: pcor
!
       allocate( pcor%crt_name(pcor%num_crt) )
       allocate( pcor%crt_comp(pcor%num_crt) )
       allocate( pcor%ifield_crt(pcor%num_crt) )
       allocate( pcor%ifield_crt2(pcor%num_crt) )
       allocate( pcor%icomp_crt(pcor%num_crt) )
!
       pcor%ifield_crt = 0
       pcor%ifield_crt2 = 0
       pcor%icomp_crt = 0
!
       end subroutine alloc_correlate_name
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_correlate_4_plane(pcor)
!
      type(correlate_4_plane), intent(inout) :: pcor
!
!
       allocate ( pcor%phys_d1(pcor%num_domain_c*pcor%num_crt) )
       allocate ( pcor%phys_d2(pcor%num_domain_c*pcor%num_crt) )
       allocate ( pcor%ave_data(pcor%iz_max*pcor%num_crt) )
       allocate ( pcor%ave_data2(pcor%iz_max*pcor%num_crt) )
       allocate ( pcor%rms_data(pcor%iz_max*pcor%num_crt) )
       allocate ( pcor%rms_data2(pcor%iz_max*pcor%num_crt) )
       allocate ( pcor%sig_data(pcor%iz_max*pcor%num_crt) )
       allocate ( pcor%sig_data2(pcor%iz_max*pcor%num_crt) )
       allocate( pcor%x_out(pcor%kx_max) )
       allocate( pcor%y_out(pcor%ky_max) )
       allocate( pcor%z_out(pcor%iz_max) )

       pcor%phys_d1 = 0.0d0
       pcor%phys_d2 = 0.0d0
       pcor%ave_data = 0.0d0
       pcor%ave_data2 = 0.0d0
       pcor%rms_data = 0.0d0
       pcor%rms_data2 = 0.0d0
       pcor%sig_data = 0.0d0
       pcor%sig_data2 = 0.0d0
       pcor%x_out = 0.0d0
       pcor%y_out = 0.0d0
       pcor%z_out = 0.0d0
!
       end subroutine alloc_correlate_4_plane
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_correlate_4_evo(pcor)
!
      type(correlate_4_plane), intent(inout) :: pcor
!
       allocate( pcor%crt_data(pcor%iz_max*pcor%num_crt) )
       allocate( pcor%rms_ratio(pcor%iz_max*pcor%num_crt) )
!
       pcor%crt_data =  0.0d0
       pcor%rms_ratio = 0.0d0
!
       end subroutine alloc_correlate_4_evo
!
!  ---------------------------------------------------------------------
!
      end module t_correlate_4_plane
