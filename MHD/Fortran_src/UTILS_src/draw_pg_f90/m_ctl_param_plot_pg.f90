!
!      module m_ctl_param_plot_pg
!
      module m_ctl_param_plot_pg
!
!      Written by H. Matsui
!
      use m_precision
      use m_field_file_format
!
      implicit none
!
!
      integer(kind = kint), parameter :: num_color_pg = 255
!
      integer(kind = kint) :: idisp_mode =    1
      integer(kind = kint) :: icolor_mode =   1
!
      integer(kind = kint) :: ist_pg, ied_pg, inc_pg
      real(kind = kreal) :: start_time_pg
      real(kind = kreal) :: delta_time_pg
!
      integer(kind = kint) :: npanel_window = 1
      real(kind = kreal) ::  flame
      real(kind = kreal) :: time
!
!    parameter for PSF file
!
      character(len=kchara) :: pg_psf_file_prefix
      integer(kind = kint) :: iflag_pg_psf_fmt = iflag_udt
!
!    parameter for z_plane
!
      real(kind = kreal) :: shell_size
      real(kind = kreal) :: shell_ratio
!
!   parameter for plane model
!
      real(kind = kreal) :: plane_size(2)
!
!   parameter for map
!
      integer(kind = kint) :: id_shell_mode_pg
      integer(kind = kint) :: num_sph_grid(2)
      integer(kind = kint) :: id_radial
      real(kind = kreal) :: r_sph
!
!
      integer(kind = kint) :: ntot_plot_pg
!
      character(len=kchara) :: fhead_map_grid
      character(len=kchara), allocatable :: fhead_image_data(:)
!
      character(len=kchara), allocatable :: field_name_4_plot(:)
      character(len=kchara), allocatable :: comp_name_4_plot(:)
      character(len=kchara), allocatable :: field_label_4_plot(:)
!
      integer(kind = kint), allocatable :: id_field_4_plot(:)
      integer(kind = kint), allocatable :: id_comp_4_plot(:)
      integer(kind = kint), allocatable :: num_comp_4_plot(:)
      integer(kind = kint), allocatable :: ncomp_org_4_plot(:)
      character(len=kchara), allocatable :: viz_name_4_plot(:)
!
      integer(kind = kint), allocatable :: num_line_pg(:)
      integer(kind = kint), allocatable :: nskip_vect_pg(:)
      real(kind = kreal), allocatable :: range_pg(:,:)
      real(kind = kreal), allocatable :: scale_pg(:)
!
!      subroutine allocate_plot_param_pg
!      subroutine deallocate_plot_param_pg
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_plot_param_pg
!
!
      allocate( fhead_image_data(ntot_plot_pg) )
!
      allocate( field_name_4_plot(ntot_plot_pg) )
      allocate( comp_name_4_plot(ntot_plot_pg) )
      allocate( field_label_4_plot(ntot_plot_pg) )
      allocate( id_field_4_plot(ntot_plot_pg) )
      allocate( id_comp_4_plot(ntot_plot_pg) )
      allocate( num_comp_4_plot(ntot_plot_pg) )
      allocate( ncomp_org_4_plot(ntot_plot_pg) )
      allocate( viz_name_4_plot(ntot_plot_pg) )
!
      allocate( num_line_pg(ntot_plot_pg) )
      allocate( nskip_vect_pg(ntot_plot_pg) )
!
      allocate( range_pg(2,ntot_plot_pg) )
      allocate( scale_pg(ntot_plot_pg) )
!
      id_field_4_plot =  0
      id_comp_4_plot =   0
      num_comp_4_plot =  0
      ncomp_org_4_plot = 0
!
      num_line_pg =   0
      nskip_vect_pg = 0
      range_pg = 0.0d0
      scale_pg = 0.0d0
!
      end subroutine allocate_plot_param_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_plot_param_pg
!
!
      deallocate( fhead_image_data )
!
      deallocate( field_name_4_plot )
      deallocate( comp_name_4_plot )
      deallocate( field_label_4_plot )
      deallocate( id_field_4_plot )
      deallocate( id_comp_4_plot )
      deallocate( num_comp_4_plot )
      deallocate( ncomp_org_4_plot )
      deallocate( viz_name_4_plot )
!
      deallocate( num_line_pg )
      deallocate( nskip_vect_pg )
!
      deallocate( range_pg )
      deallocate( scale_pg )
!
      end subroutine deallocate_plot_param_pg
!
!-----------------------------------------------------------------------
!
      end module m_ctl_param_plot_pg
